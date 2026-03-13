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


-export([
  start_link/0, start_link/2, start_link/3,
  start/0, start/2, start/3,
  stop/1,
  now/1,
  update/2,
  timestamp/1,
  set_maxoffset/2,
  get_maxoffset/1,
  less/2,
  equal/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([physical_clock/0]).
-export([manual_clock/0, manual_clock/1,
         set_manual_clock/2, stop_manual_clock/1]).

-include("hlc.hrl").

-type clock() :: pid().
-type timestamp() :: #timestamp{}.
-type clock_fun() :: fun().

-export_type([clock/0,
              timestamp/0,
              clock_fun/0]).


%% state
-record(clock, {
  phys_clock,
  ts,

  %% The maximal offset of the HLC's wall time from the underlying physical
	%% clock. A well-chosen value is large enough to ignore a reasonable amount
	%% of clock skew but will prevent ill-configured nodes from dramatically
	%% skewing the wall time of the clock into the future.
  maxoffset = 0,

  %% monotonicityErrorsCount indicate how often this clock was
  %% observed to jump backwards.
  monotonicity_errors_count = 0,
  
  %% lastPhysicalTime reports the last measured physical time. This
  %% is used to detect clock jumps.
  last_physical_time = 0
}).


%% @doc start a new hybrid logical clock with physical clock and maxoffset=0
-spec start_link() -> {ok, clock()}.
start_link() -> start_link(fun physical_clock/0, 0).

%% @doc start a new hybrid logical clock with a custom physical clock function.
-spec start_link(ClockFun :: clock_fun(), MaxOffset :: non_neg_integer()) -> {ok, clock()}.
start_link(ClockFun, MaxOffset) ->
  gen_server:start_link(?MODULE, [{ClockFun, MaxOffset}], []).

%% @doc start a new hybrid logical clock with a name. Clocks are always local
-spec start_link(Name :: atom(), ClockFun :: clock_fun(), MaxOffset :: non_neg_integer()) -> {ok, clock()}.
start_link(Name, ClockFun, MaxOffset) ->
  gen_server:start_link({local, Name}, ?MODULE, [{ClockFun, MaxOffset}], []).

-spec start() -> {ok, clock()}.
start() -> start(fun physical_clock/0, 0).

-spec start(ClockFun :: clock_fun(), MaxOffset :: non_neg_integer()) -> {ok, clock()}.
start(ClockFun, MaxOffset) ->
  gen_server:start(?MODULE, [{ClockFun, MaxOffset}], []).

-spec start(Name :: atom(), ClockFun :: clock_fun(), MaxOffset :: non_neg_integer()) -> {ok, clock()}.
start(Name, ClockFun, MaxOffset) ->
  gen_server:start({local, Name}, ?MODULE, [{ClockFun, MaxOffset}], []).

%% @doc stop a clock
-spec stop(Clock :: clock()) -> ok | {error, term()}.
stop(Clock) -> gen_server:stop(Clock).


%% @doc  returns a timestamp associated with an event from the local
%% machine that may be sent to other members of the distributed network.
%% This is the counterpart of Update, which is passed a timestamp
%% received from another member of the distributed network.
-spec now(clock()) -> timestamp().
now(Clock) ->
  gen_server:call(Clock, now).

%% @doc takes a hybrid timestamp, usually originating from an event
%% received from another member of a distributed system. The clock is
%% updated and the hybrid timestamp  associated to the receipt of the
%% event returned.  An error may only occur if offset checking is active
%% and  the remote timestamp was rejected due to clock offset,  in which
%% case the state of the clock will not have been  altered. To timestamp
%% events of local origin, use Now instead.
-spec update(clock(), timestamp()) ->
  {ok, timestamp()}
  | {timeahead, timestamp()}.
update(Clock, RT) ->
  gen_server:call(Clock, {update, RT}).

%% @doc return a copy of the clock timestamp without adjusting it
-spec timestamp(clock()) -> timestamp().
timestamp(Clock) ->
  gen_server:call(Clock, timestamp).

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
  gen_server:call(Clock,{maxoffset, Offset});
set_maxoffset(_, _) ->
  error(badarg).

%% @doc returns the maximal offset allowed.
%%  A value of 0 means offset checking is disabled.
-spec get_maxoffset(clock()) -> non_neg_integer().
get_maxoffset(Clock) ->
  gen_server:call(Clock, maxoffset).

%% @doc timestamp in milliseconds
-spec physical_clock() -> non_neg_integer().
physical_clock() ->
  erlang:system_time(millisecond).

%% @doc create a manually controlled physical clock
-spec manual_clock() -> {pid(), fun()}.
manual_clock() ->
  manual_clock(0).

%% @doc create a manually controlled physical clock and initialise it
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

%% @doc change the value of the manually controlled physical clock.
-spec set_manual_clock(pid(), integer()) -> ok.
set_manual_clock(Pid, TS) ->
  Pid ! {update_ts, TS},
  ok.

%% @doc stop a manually controlled physical clock.
-spec stop_manual_clock(pid()) -> ok.
stop_manual_clock(Pid) ->
  Pid ! stop,
  ok.


%% @doc compare if one timestamps happen before the other
-spec less(timestamp(), timestamp()) -> boolean().
less(#timestamp{wall_time=W, logical=LA},
     #timestamp{wall_time=W, logical=LB}) when LA < LB ->
  true;
less(#timestamp{wall_time=WA}, #timestamp{wall_time=WB}) when WA < WB ->
  true;
less(_, _) ->
  false.


%% @doc compare if 2 timestamps are equal
-spec equal(timestamp(), timestamp()) -> boolean().
equal(TS, TS) -> true;
equal(_, _) -> false.


%% -------------------
%% gen_server callbacks

%% @private
init([{ClockFun, MaxOffset}]) ->
  {ok, #clock{phys_clock=ClockFun, ts=#timestamp{}, maxoffset=MaxOffset}}.

%% @private
handle_call(maxoffset, _From, Clock = #clock{maxoffset=MaxOffset}) ->
  {reply, MaxOffset, Clock};

handle_call({maxoffset, MaxOffset}, _From, Clock) ->
  {reply, ok, Clock#clock{maxoffset=MaxOffset}};

handle_call(now, _From, Clock0 = #clock{ts=TS}) ->
  {Now, Clock1} = get_physclock(Clock0),
  NewTS = if
            TS#timestamp.wall_time >= Now ->
              TS#timestamp{logical=incr_logical(TS#timestamp.logical)};
            true ->
              TS#timestamp{wall_time=Now, logical=0}
          end,
  {reply, NewTS, Clock1#clock{ts=NewTS}};

handle_call({update, RT}, _From, Clock) ->
  {Reply, NewClock} = update_clock(RT, Clock),
  {reply, Reply, NewClock};

handle_call(physical_clock, _From, Clock0) ->
  {Time, Clock1} = get_physclock(Clock0),
  {reply, Time, Clock1};

handle_call(timestamp, _From, Clock = #clock{ts = TS}) ->
  {reply, TS, Clock};

handle_call(_Msg, _From, Clock) ->
  {reply, bad_call, Clock}.

%% @private
handle_cast(_Msg, Clock) -> {noreply, Clock}.

%% @private
handle_info(_Msg, Clock) -> {noreply, Clock}.

%% @private
terminate(_Reason, _Clock) -> ok.

%% @private
code_change(_OldVsn, Clock, _Extra) -> {ok, Clock}.


%% -------------------
%% internals

%% Maximum logical counter value (2^31 - 1)
-define(MAX_LOGICAL, 16#7FFFFFFF).

%% @doc Increment logical counter with overflow protection.
%% Raises error if counter would overflow.
-spec incr_logical(non_neg_integer()) -> non_neg_integer().
incr_logical(Logical) when Logical >= ?MAX_LOGICAL ->
  error({logical_overflow, Logical});
incr_logical(Logical) ->
  Logical + 1.

update_clock(RT, #clock{ts=TS, maxoffset=MaxOffset} = Clock0) ->
  {Now, Clock1} = get_physclock(Clock0),
  
  #timestamp{wall_time=RTWalltime, logical=RTLogical} = RT,
  #timestamp{wall_time=TSWalltime, logical=TSLogical} = TS,
  
  Offset = RTWalltime - Now,
  
  %% test if physical clock is ahead of both wall times.
  NowIsAhead = ((Now > TSWalltime) and (Now > RTWalltime)),
  
  case NowIsAhead of
    true ->
      %% set new wall_time and reset logical clock
      NewTS = TS#timestamp{wall_time=Now, logical=0},
      {{ok, NewTS}, Clock1#clock{ts=NewTS}};
    false when RTWalltime > TSWalltime ->
      if
        ((MaxOffset > 0) and (Offset > MaxOffset)) ->
          _ = error_logger:info_msg(
            "~s: remote wall time offset from local physical clock: ~p (~p ahead)~n",
            [?MODULE_STRING, RTWalltime, Offset]
          ),
          {{timeahead, TS}, Clock1};
        true ->
          NewTS = TS#timestamp{wall_time=RTWalltime,
                               logical = incr_logical(RTLogical)},
          {{ok, NewTS}, Clock1#clock{ts=NewTS}}
      end;
    false when TSWalltime > RTWalltime ->
      NewTS = TS#timestamp{logical=incr_logical(TSLogical)},
      {{ok, NewTS}, Clock1#clock{ts=NewTS}};
    false ->
      TSLogical1 = if RTLogical > TSLogical -> RTLogical;
                     true -> TSLogical
                   end,
      NewTS = TS#timestamp{logical=incr_logical(TSLogical1)},
      {{ok, NewTS}, Clock1#clock{ts=NewTS}}
  end.

get_physclock(#clock{phys_clock=PhysClock,
                     maxoffset=MaxOffset,
                     last_physical_time=Last} = Clock) when Last =/= 0 ->
  Now = PhysClock(),
  Interval = Last - Now,
  if
    (Interval > (MaxOffset / 10)) ->
      _ = error_logger:error_msg(
        "~s, backward time jump detected: ~p~n",
        [?MODULE_STRING, Interval]
      ),
      Clock2 = Clock#clock{
        monotonicity_errors_count = Clock#clock.monotonicity_errors_count +1,
        last_physical_time = Now
      },
      {Now, Clock2};
    true ->
      {Now, Clock#clock{last_physical_time=Now}}
  end;
get_physclock(#clock{phys_clock=PhysClock} = Clock) ->
  Now = PhysClock(),
  {Now, Clock#clock{last_physical_time=Now}}.


manual_clock_loop(Last) ->
  receive
    {req_ts, From} ->
      From ! {new_ts, Last},
      manual_clock_loop(Last);
    {update_ts, TS} ->
      manual_clock_loop(TS);
    stop ->
      ok
  end.

