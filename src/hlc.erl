%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%
%% @doc
%% hlc
%%
%% implements the Hybrid Logical Clock outlined in
%% "Logical Physical Clocks and Consistent Snapshots in Globally
%% Distributed Databases", available online at
%% http://www.cse.buffalo.edu/tech-reports/2014-04.pdf.
%%
%% An hybrid logical clock is available as a linked process.   Objects of this
%% type model causality while maintaining a relation  to physical time.
%% Roughly speaking, timestamps  consist of the largest wall clock time among
%% all  events, and a logical clock that ticks whenever  an event happens in
%% the future of the local physical  clock.
%%
-module(hlc).
-behaviour(gen_server).

-include("hlc.hrl").

-export([new/0, new/1,
         close/1,
         set_maxdrift/2,
         get_maxdrift/1,
         timestamp/1,
         now/1,
         update/2]).

-export([physical_clock/0]).
-export([manual_clock/0, manual_clock/1,
         set_manual_clock/2]).

-export([ts_less/2, ts_equal/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-record(clock, {physical_clock,
                ts,
                maxdrift}).

-type timestamp() :: #timestamp{}.
-export_type([timestamp/0]).


%% @doc create a new hybrid logical clock.
-spec new() -> {ok, pid()} | {error, any()}.
new() ->
    new(fun physical_clock/0).

%% @doc create a new hybrid logical clock with a custom physical clock function.
-spec new(fun()) -> {ok, pid()} | {error, any()}.
new(ClockFun) ->
    gen_server:start_link(?MODULE, [ClockFun], []).

%% close the hybrid logical clock
-spec close(pid) -> ok.
close(Ref) ->
    gen_server:call(Ref, stop).


%% @doc Sets the maximal drift from the physical clock that a call to
%% Update may cause. A well-chosen value is large enough to ignore a
%% reasonable amount of clock skew but will prevent ill-configured nodes
%% from dramatically skewing the wall time of the clock into the future.
%%
%%
%% A value of zero disables this safety feature.  The default value for
%% a new instance is zero.
-spec set_maxdrift(pid(), integer()) -> ok.
set_maxdrift(Ref, MaxDrift) ->
    gen_server:call(Ref, {set_maxdrift, MaxDrift}).


%% @doc returns the maximal drift allowed.
%%  A value of 0 means drift checking is disabled.
-spec get_maxdrift(pid()) -> integer().
get_maxdrift(Ref) ->
    gen_server:call(Ref, get_maxdrift).

%% @doc return a copy of the clock timestamp without adjusting it
-spec timestamp(pid()) -> timestamp().
timestamp(Ref) ->
    gen_server:call(Ref, timestamp).

%% @doc  returns a timestamp associated with an event from the local
%% machine that may be sent to other members of the distributed network.
%% This is the counterpart of Update, which is passed a timestamp
%% received from another member of the distributed network.
-spec now(pid()) -> timestamp().
now(Ref) ->
    gen_server:call(Ref, now).


%% @doc takes a hybrid timestamp, usually originating from an event
%% received from another member of a distributed system. The clock is
%% updated and the hybrid timestamp  associated to the receipt of the
%% event returned.  An error may only occur if drift checking is active
%% and  the remote timestamp was rejected due to clock drift,  in which
%% case the state of the clock will not have been  altered. To timestamp
%% events of local origin, use Now instead.
-spec update(pid(), timestamp()) ->
    timestamp()
    | {error, {time_ahead, timestamp()}}.
update(Ref, RT) ->
    gen_server:call(Ref, {update, RT}).


%% @doc create a manually controlled physicl clock
-spec manual_clock() -> {pid(), fun()}.
manual_clock() ->
    manual_clock(0).

%% @doc create a manually controlled physicl clock and initialise it
%% with a default ts.
-spec manual_clock(integer()) -> {pid(), fun()}.
manual_clock(TS0) ->
    Pid = spawn_link(fun() -> manual_clock_loop(TS0) end),

    UserFun = fun() ->
            Pid ! {req_ts, self()},
            receive
                {new_ts, TS} -> TS
            end
    end,
    {Pid, UserFun}.

%% @doc change the value of the manually controlled physicall clock.
-spec set_manual_clock(pid(), integer()) -> ok.
set_manual_clock(Pid, TS) ->
    Pid ! {update_ts, TS},
    ok.

%% @doc compare if one timestamps happen before the other
-spec ts_less(timestamp(), timestamp()) -> true | false.
ts_less(#timestamp{wall_time=W, logical=LA},
               #timestamp{wall_time=W, logical=LB}) when LA < LB ->
    true;
ts_less(#timestamp{wall_time=WA}, #timestamp{wall_time=WB})
        when WA < WB ->
    true;
ts_less(_, _) ->
    false.


%% compare if 2 timestamps are equal
-spec ts_equal(timestamp(), timestamp()) -> true | false.
ts_equal(TS, TS) ->
    true;
ts_equal(_, _) ->
    false.



%% ---------------------
%% gen server callbacks
%% ---------------------

%% @private
init([Fun]) ->
    {ok, #clock{physical_clock=Fun,
                ts = #timestamp{},
                maxdrift = 0}}.

%% @private
handle_call({set_maxdrift, MaxDrift}, _From, Clock) ->
    {reply, ok, Clock#clock{maxdrift=MaxDrift}};

handle_call(get_maxdrift, _From, #clock{maxdrift=MaxDrift}=Clock) ->
    {reply, MaxDrift, Clock};

handle_call(timestamp, _From, #clock{ts=TS}=Clock) ->
    {reply, TS, Clock};

handle_call(now, _From, #clock{physical_clock=PhysicalCLock, ts=TS}=Clock) ->
    Now = PhysicalCLock(),
    NewTS = if TS#timestamp.wall_time >= Now ->
            TS#timestamp{logical=TS#timestamp.logical + 1};
        true ->
            TS#timestamp{wall_time=Now,
                         logical=0}
    end,

    {reply, NewTS, Clock#clock{ts=NewTS}};

handle_call({update, RT}, _From, #clock{physical_clock=PhysicalCLock,
                                        ts=TS, maxdrift=MaxDrift}=Clock)  ->
    Now = PhysicalCLock(),

    #timestamp{wall_time=RTWalltime,
               logical=RTLogical} = RT,
    #timestamp{wall_time=TSWalltime,
               logical=TSLogical} = TS,

    Drift = RTWalltime - Now,

    %% test if physical clock is ahead of both wall times.
    NowIsAhead = ((Now > TSWalltime) and (Now > RTWalltime)),

    case NowIsAhead of
        true ->
            %% set new wall_time and reset logical clock
            NewTS = TS#timestamp{wall_time=Now, logical=0},
            {reply, NewTS, Clock#clock{ts=NewTS}};

        false when RTWalltime > TSWalltime ->
            if ((MaxDrift > 0) and (Drift > MaxDrift)) ->
                    error_logger:error_msg("Remote wall time drifts from
                                localphysical clock: %p (%p ahead)",
                                [RTWalltime, Drift]),

                    {reply, {error, {time_ahead, TS}}, Clock};
                true ->
                    NewTS = TS#timestamp{wall_time=RTWalltime,
                                         logical = RTLogical +1},
                    {reply, NewTS, Clock#clock{ts=NewTS}}
            end;
        false when TSWalltime > RTWalltime ->
            NewTS = TS#timestamp{logical=TSLogical +1},
            {reply, NewTS, Clock#clock{ts=NewTS}};
        false ->
            TSLogical1 = if RTLogical > TSLogical -> RTLogical;
                true -> TSLogical
            end,
            NewTS = TS#timestamp{logical=TSLogical1 +1},
            {reply, NewTS, Clock#clock{ts=NewTS}}
    end.

%% @private
handle_cast(_Msg, Clock) ->
    {noreply, Clock}.

%% @private
handle_info(_Info, Clock) ->
    {noreply, Clock}.

%% @private
code_change(_OldVsn, Clock, _Extra) ->
    {ok, Clock}.

%% @private
terminate(_Reason, _Clock) ->
    ok.


%% timestamp in milliseconds
physical_clock() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.


manual_clock_loop(Last) ->
    receive
        {req_ts, From} ->
            From ! {new_ts, Last},
            manual_clock_loop(Last);
        {update_ts, TS} ->
            manual_clock_loop(TS)
    end.
