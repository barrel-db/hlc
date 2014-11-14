%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%

-module(hlc_tests).

-include_lib("include/hlc.hrl").
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
    {MClock, MClockFun} = hlc:manual_clock(),
    {ok, C} = hlc:new(MClockFun),
    hlc:set_maxdrift(C, 1000),

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
            ({WallClock, send, _Input, Expected}=Case) ->
                ?debugFmt(<<" case ~p~n">>, [Case]),
                hlc:set_manual_clock(MClock, WallClock),
                Current = hlc:now(C),
                ?debugFmt(<<"current is ~p~n">>, [Current]),
                ?assertMatch(Current, Expected);
            ({WallClock, recv, Input, Expected}=Case) ->
                ?debugFmt(<<" case ~p~n">>, [Case]),
                hlc:set_manual_clock(MClock, WallClock),
                Previous = hlc:timestamp(C),
                case hlc:update(C, Input) of
                    {error, {_Reason, Current}} ->
                        ?debugFmt(<<"current is ~p~n">>, [Current]),
                        ?assertMatch(Current, Expected);
                    Current ->
                        ?debugFmt(<<"current is ~p~n">>, [Current]),
                        ?assert(Current /= Previous),
                        ?assertMatch(Current, Expected)
                end
        end, Cases).

set_maxdrift_test() ->
    {_MClock, MClockFun} = hlc:manual_clock(123456789),
    SkewedTime = 123456789 + 51,
    {ok, C} = hlc:new(MClockFun),

    ?assert(hlc:get_maxdrift(C) =:= 0),
    hlc:set_maxdrift(C, 50),
    ?assert(hlc:get_maxdrift(C) =:= 50),

    hlc:now(C),
    TS = hlc:timestamp(C),
    ?assert(TS#timestamp.wall_time =:= 123456789),

    ?assertMatch({error, _}, hlc:update(C, ?ts(SkewedTime, 0))),

    hlc:set_maxdrift(C, 0),
    ?assertMatch(#timestamp{wall_time=SkewedTime},
                 hlc:update(C, ?ts(SkewedTime, 0))).
