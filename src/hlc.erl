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

%% We don't want warnings about the use of erlang:now/0 in
%% %% this module.
-compile(nowarn_deprecated_function).

-behaviour(gen_server).

-include("hlc.hrl").

-export([new/0, new/1,
         close/1,
         set_maxoffset/2,
         get_maxoffset/1,
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
                maxoffset}).

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


%% @doc Sets the maximal offset from the physical clock that a call to
%% Update may cause. A well-chosen value is large enough to ignore a
%% reasonable amount of clock skew but will prevent ill-configured nodes
%% from dramatically skewing the wall time of the clock into the future.
%%
%%
%% A value of zero disables this safety feature.  The default value for
%% a new instance is zero.
-spec set_maxoffset(pid(), integer()) -> ok.
set_maxoffset(Ref, MaxOffset) ->
    gen_server:call(Ref, {set_maxoffset, MaxOffset}).


%% @doc returns the maximal offset allowed.
%%  A value of 0 means offset checking is disabled.
-spec get_maxoffset(pid()) -> integer().
get_maxoffset(Ref) ->
    gen_server:call(Ref, get_maxoffset).

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
%% event returned.  An error may only occur if offset checking is active
%% and  the remote timestamp was rejected due to clock offset,  in which
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
                maxoffset = 0}}.

%% @private
handle_call({set_maxoffset, MaxOffset}, _From, Clock) ->
    {reply, ok, Clock#clock{maxoffset=MaxOffset}};

handle_call(get_maxoffset, _From, #clock{maxoffset=MaxOffset}=Clock) ->
    {reply, MaxOffset, Clock};

handle_call(timestamp, _From, #clock{ts=TS}=Clock) ->
    {reply, TS, Clock};

handle_call(now, _From, #clock{physical_clock=PhysicalCLock, ts=TS}=Clock) ->
    Now = PhysicalCLock(),
    NewTS = if TS#timestamp.wall_time >= Now ->
            TS#timestamp{logical=TS#timestamp.logical + 1};
        true ->
            TS#timestamp{wall_time=Now, logical=0}
    end,

    {reply, NewTS, Clock#clock{ts=NewTS}};

handle_call({update, RT}, _From, #clock{physical_clock=PhysicalCLock,
                                        ts=TS, maxoffset=MaxOffset}=Clock)  ->
    Now = PhysicalCLock(),

    #timestamp{wall_time=RTWalltime, logical=RTLogical} = RT,
    #timestamp{wall_time=TSWalltime, logical=TSLogical} = TS,

    Drift = RTWalltime - Now,

    %% test if physical clock is ahead of both wall times.
    NowIsAhead = ((Now > TSWalltime) and (Now > RTWalltime)),

    case NowIsAhead of
        true ->
            %% set new wall_time and reset logical clock
            NewTS = TS#timestamp{wall_time=Now, logical=0},
            {reply, NewTS, Clock#clock{ts=NewTS}};

        false when RTWalltime > TSWalltime ->
            if ((MaxOffset > 0) and (Drift > MaxOffset)) ->
                    error_logger:info_msg("Remote wall time offsets from
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
    {Mega,Sec,Micro} = erlang_ts(),
    (Mega*1000000+Sec)*1000000+Micro.


erlang_ts() ->
    try
        erlang:timestamp()
    catch
        error:undef ->
            erlang:now()
    end.


manual_clock_loop(Last) ->
    receive
        {req_ts, From} ->
            From ! {new_ts, Last},
            manual_clock_loop(Last);
        {update_ts, TS} ->
            manual_clock_loop(TS)
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ts(W, L), #timestamp{wall_time=W, logical=L}).

basic_test() ->
    {ok, C} = hlc:new(),
    S = hlc:now(C),
    timer:sleep(5),
    T = #timestamp{wall_time=hlc:physical_clock()},

    ?assertMatch(true, hlc:ts_less(S, T)),
    ?assertMatch(false, hlc:ts_less(T, S)),
    ?assert(T#timestamp.wall_time > S#timestamp.wall_time),
    ?assert(S#timestamp.logical =:= 0).

manual_clock_test() ->
    {Pid, Fun} = hlc:manual_clock(),
    ?assert(Fun() =:= 0),
    ok = hlc:set_manual_clock(Pid, 1),
    ?assert(Fun() =:= 1),
    ok = hlc:set_manual_clock(Pid, 2),
    ?assert(Fun() =:= 2).

less_test() ->
    {MClock, MClockFun} = hlc:manual_clock(),
    {ok, C} = hlc:new(MClockFun),

    A = hlc:timestamp(C),
    B = hlc:timestamp(C),

    ?assert(A =:= B),

    hlc:set_manual_clock(MClock, 1),
    B1 = hlc:now(C),
    ?assertMatch(true, hlc:ts_less(A, B1)).

equal_test() ->
    {MClock, MClockFun} = hlc:manual_clock(),
    {ok, C} = hlc:new(MClockFun),

    A = hlc:timestamp(C),
    B = hlc:timestamp(C),

    ?assertMatch(true, hlc:ts_equal(A, B)),

    hlc:set_manual_clock(MClock, 1),
    B1 = hlc:now(C),
    ?assertMatch(false, hlc:ts_equal(A, B1)).

clock_test() ->
    error_logger:tty(false),
    {MClock, MClockFun} = hlc:manual_clock(),
    {ok, C} = hlc:new(MClockFun),
    hlc:set_maxoffset(C, 1000),

    Cases = [{5, send, nil, ?ts(5,0)},
             {6, send, nil, ?ts(6,0)},
             {10, recv, ?ts(10,5), ?ts(10, 6)},
             {7, send, nil, ?ts(10, 7)},
             {8, recv, ?ts(10, 4), ?ts(10, 8)},
             {9, recv, ?ts(1100, 888), ?ts(10, 8)},
             {10, recv, ?ts(10, 99), ?ts(10, 100)},
             {11, recv, ?ts(10, 31), ?ts(11, 0)},
             {11, send, nil, ?ts(11, 1)}],

    lists:foreach(fun
            ({WallClock, send, _Input, Expected}) ->
                hlc:set_manual_clock(MClock, WallClock),
                Current = hlc:now(C),
                ?assertMatch(Current, Expected);
            ({WallClock, recv, Input, Expected}) ->
                hlc:set_manual_clock(MClock, WallClock),
                Previous = hlc:timestamp(C),
                case hlc:update(C, Input) of
                    {error, {_Reason, Current}} ->
                        ?assertMatch(Current, Expected);
                    Current ->
                        ?assert(Current /= Previous),
                        ?assertMatch(Current, Expected)
                end
        end, Cases).

set_maxoffset_test() ->
    error_logger:tty(false),
    {_MClock, MClockFun} = hlc:manual_clock(123456789),
    SkewedTime = 123456789 + 51,
    {ok, C} = hlc:new(MClockFun),

    ?assert(hlc:get_maxoffset(C) =:= 0),
    hlc:set_maxoffset(C, 50),
    ?assert(hlc:get_maxoffset(C) =:= 50),

    hlc:now(C),
    TS = hlc:timestamp(C),
    ?assert(TS#timestamp.wall_time =:= 123456789),

    ?assertMatch({error, _}, hlc:update(C, ?ts(SkewedTime, 0))),

    hlc:set_maxoffset(C, 0),
    ?assertMatch(#timestamp{wall_time=SkewedTime},
                 hlc:update(C, ?ts(SkewedTime, 0))).

-endif.
