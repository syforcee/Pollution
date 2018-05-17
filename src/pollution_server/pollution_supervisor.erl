%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 13:18
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("pawel").

%% API
-export([start/0, init/0]).

start() ->
  register(pollutionSupervisor, spawn(pollution_supervisor, init, [])).

init() ->
  process_flag(trap_exit, true),
  io: format("pollution_supervisor started~n"),
  loop().

loop()->
  pollution_server:start(),
  io:format("pollution_server started~n"),
  receive
    {'EXIT', _, _} -> io:format("pollution_server crashed~n"),
                      loop();
    _ -> io:format("pollution_server killed~n")
  end.