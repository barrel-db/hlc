%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.


-record(timestamp, {wall_time = 0,
                    logical = 0}).

-define(MAX_TIMESTAMP, #timestamp{wall_time = 1 bsl 63 -1,
                                  logical = 1 bsl 31 - 1}).

-define(MIN_TIMESTAMP, #timestamp{wall_time = 0,
                                  logical = 0}).
