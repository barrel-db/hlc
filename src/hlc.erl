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
-include("hlc.hrl").

-export([new/0, new/1,
         now/1,
         update/2,
         timestamp/1,
         set_maxoffset/2,
         get_maxoffset/1,
         less/2,
         equal/2]).

-export([physical_clock/0]).
-export([manual_clock/0, manual_clock/1,
         set_manual_clock/2]).

-record(clock, {phys_clock,
                ts,
                maxoffset}).

-type clock() :: #clock{}.
-type timestamp() :: #timestamp{}.
-export_type([clock/0,
              timestamp/0]).


%% @doc create a new hybrid logical clock.
-spec new() ->  clock().
new() ->
    new(fun physical_clock/0).

%% @doc create a new hybrid logical clock with a custom physical clock function.
-spec new(fun()) -> clock().
new(ClockFun) ->
    #clock{phys_clock=ClockFun, ts=#timestamp{}, maxoffset=0}.

%% @doc  returns a timestamp associated with an event from the local
%% machine that may be sent to other members of the distributed network.
%% This is the counterpart of Update, which is passed a timestamp
%% received from another member of the distributed network.
-spec now(clock()) -> {timestamp(), clock()}.
now(#clock{phys_clock=PhysClock, ts=TS} = Clock) ->
    Now = PhysClock(),
    NewTS = if TS#timestamp.wall_time >= Now ->
            TS#timestamp{logical=TS#timestamp.logical + 1};
        true ->
            TS#timestamp{wall_time=Now, logical=0}
    end,
    {NewTS, Clock#clock{ts=NewTS}}.

%% @doc takes a hybrid timestamp, usually originating from an event
%% received from another member of a distributed system. The clock is
%% updated and the hybrid timestamp  associated to the receipt of the
%% event returned.  An error may only occur if offset checking is active
%% and  the remote timestamp was rejected due to clock offset,  in which
%% case the state of the clock will not have been  altered. To timestamp
%% events of local origin, use Now instead.
-spec update(timestamp(), clock()) ->
    {ok, timestamp(), clock()}
    | {timeahead, timestamp()}.
update(RT, #clock{phys_clock=PhysClock, ts=TS, maxoffset=MaxOffset} = Clock) ->
    Now = PhysClock(),

    #timestamp{wall_time=RTWalltime, logical=RTLogical} = RT,
    #timestamp{wall_time=TSWalltime, logical=TSLogical} = TS,

    Offset = RTWalltime - Now,

    %% test if physical clock is ahead of both wall times.
    NowIsAhead = ((Now > TSWalltime) and (Now > RTWalltime)),

    case NowIsAhead of
        true ->
            %% set new wall_time and reset logical clock
            NewTS = TS#timestamp{wall_time=Now, logical=0},
            {ok, NewTS, Clock#clock{ts=NewTS}};

        false when RTWalltime > TSWalltime ->
            if ((MaxOffset > 0) and (Offset > MaxOffset)) ->
                    error_logger:info_msg("Remote wall time offsets from
                                localphysical clock: %p (%p ahead)",
                                [RTWalltime, Offset]),

                    {timeahead, TS};
                true ->
                    NewTS = TS#timestamp{wall_time=RTWalltime,
                                         logical = RTLogical +1},
                    {ok, NewTS, Clock#clock{ts=NewTS}}
            end;
        false when TSWalltime > RTWalltime ->
            NewTS = TS#timestamp{logical=TSLogical +1},
            {ok, NewTS, Clock#clock{ts=NewTS}};
        false ->
            TSLogical1 = if RTLogical > TSLogical -> RTLogical;
                true -> TSLogical
            end,
            NewTS = TS#timestamp{logical=TSLogical1 +1},
            {ok, NewTS, Clock#clock{ts=NewTS}}
    end.


%% @doc return a copy of the clock timestamp without adjusting it
-spec timestamp(clock()) -> timestamp().
timestamp(#clock{ts=TS}) -> TS.

%% @doc compare if one timestamps happen before the other
-spec less(timestamp(), timestamp()) -> true | false.
less(#timestamp{wall_time=W, logical=LA},
               #timestamp{wall_time=W, logical=LB}) when LA < LB ->
    true;
less(#timestamp{wall_time=WA}, #timestamp{wall_time=WB})
        when WA < WB ->
    true;
less(_, _) ->
    false.


%% @doc compare if 2 timestamps are equal
-spec equal(timestamp(), timestamp()) -> true | false.
equal(TS, TS) ->
    true;
equal(_, _) ->
    false.

%% @doc Sets the maximal offset from the physical clock that a call to
%% Update may cause. A well-chosen value is large enough to ignore a
%% reasonable amount of clock skew but will prevent ill-configured nodes
%% from dramatically skewing the wall time of the clock into the future.
%%
%%
%% A value of zero disables this safety feature.  The default value for
%% a new instance is zero.
-spec set_maxoffset(non_neg_integer(), clock()) -> ok.
set_maxoffset(Offset, Clock) when Offset >= 0 ->
    Clock#clock{maxoffset=Offset};
set_maxoffset(_, _) ->
    error(badarg).

%% @doc returns the maximal offset allowed.
%%  A value of 0 means offset checking is disabled.
-spec get_maxoffset(clock()) -> non_neg_integer().
get_maxoffset(#clock{maxoffset=Offset}) ->
    Offset.

%% @doc timestamp in milliseconds
-spec physical_clock() -> non_neg_integer().
physical_clock() ->
    {Mega,Sec,Micro} = erlang_ts(),
    (Mega*1000000+Sec)*1000000+Micro.


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
    C = hlc:new(),
    {S, _} = hlc:now(C),
    timer:sleep(5),
    T = #timestamp{wall_time=hlc:physical_clock()},

    ?assertMatch(true, hlc:less(S, T)),
    ?assertMatch(false, hlc:less(T, S)),
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
    C = hlc:new(MClockFun),

    A = hlc:timestamp(C),
    B = hlc:timestamp(C),

    ?assert(A =:= B),

    hlc:set_manual_clock(MClock, 1),
    {B1, _} = hlc:now(C),
    ?assertMatch(true, hlc:less(A, B1)).

equal_test() ->
    {MClock, MClockFun} = hlc:manual_clock(),
    C = hlc:new(MClockFun),

    A = hlc:timestamp(C),
    B = hlc:timestamp(C),

    ?assertMatch(true, hlc:equal(A, B)),

    hlc:set_manual_clock(MClock, 1),
    {B1, _} = hlc:now(C),
    ?assertMatch(false, hlc:equal(A, B1)).

clock_test() ->
    error_logger:tty(false),
    {MClock, MClockFun} = hlc:manual_clock(),
    C0 = hlc:new(MClockFun),
    C = hlc:set_maxoffset(1000, C0),

    Cases = [{5, send, nil, ?ts(5,0)},
             {6, send, nil, ?ts(6,0)},
             {10, recv, ?ts(10,5), ?ts(10, 6)},
             {7, send, nil, ?ts(10, 7)},
             {8, recv, ?ts(10, 4), ?ts(10, 8)},
             {9, recv, ?ts(1100, 888), ?ts(10, 8)},
             {10, recv, ?ts(10, 99), ?ts(10, 100)},
             {11, recv, ?ts(10, 31), ?ts(11, 0)},
             {11, send, nil, ?ts(11, 1)}],

    _ = lists:foldl(fun
            ({WallClock, send, _Input, Expected}, C1) ->
                hlc:set_manual_clock(MClock, WallClock),
                {Current, C2} = hlc:now(C1),
                ?assertMatch(Current, Expected),
                C2;
            ({WallClock, recv, Input, Expected}, C1) ->
                hlc:set_manual_clock(MClock, WallClock),
                Previous = hlc:timestamp(C1),
                case hlc:update(Input, C1) of
                    {timeahead, Current} ->
                        ?assertMatch(Current, Expected),
                        C1;
                    {ok, Current, C2} ->
                        ?assert(Current /= Previous),
                        ?assertMatch(Current, Expected),
                        C2
                end
        end, C,  Cases).

set_maxoffset_test() ->
    error_logger:tty(false),
    {_MClock, MClockFun} = hlc:manual_clock(123456789),
    SkewedTime = 123456789 + 51,
    C = hlc:new(MClockFun),

    ?assert(hlc:get_maxoffset(C) =:= 0),
    C2 = hlc:set_maxoffset(50, C),
    ?assert(hlc:get_maxoffset(C2) =:= 50),

    {_, C3} = hlc:now(C2),
    TS = hlc:timestamp(C3),
    ?assert(TS#timestamp.wall_time =:= 123456789),

    ?assertMatch({timeahead, _}, hlc:update(?ts(SkewedTime, 0), C3)),

    C4 = hlc:set_maxoffset(0, C3),
    ?assertMatch({ok, #timestamp{wall_time=SkewedTime}, _},
                 hlc:update(?ts(SkewedTime, 0), C4)).

-endif.
