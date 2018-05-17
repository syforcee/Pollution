%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2018 23:26
%%%-------------------------------------------------------------------
-module(pollution_gen_supervisor).
-author("pawel").
-behavior(supervisor).

%% API
-export([start/0, start_link/0, init/1]).

start() ->
  start_link().

start_link() ->
  supervisor:start_link({local, varSupervisor}, ?MODULE, []).

init(InitValue) ->
  io:format("Supervisor started~n"),
  {ok, {
    {one_for_all, 2, 3},
    [ {pollution_gen_server,
      {pollution_gen_server, start, []},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.
