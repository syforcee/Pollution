%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 13:54
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("pawel").

%% API
-export([start_link/0, init/1, addStation/3]).

%% START %%
start_link()   ->
  M = pollution:createMonitor(),
  gen_server:start_link({local,?MODULE},?MODULE,M,[]).

%% INTERFEJS KLIENT -> SERWER %%
addStation(addStation, Name, Coords) -> gen_server:call(?MODULE, {addStation, Name, Coords}).

init(N)-> {ok,N}.

%% OBSŁUGA WIADOMOŚCI %%
%%handle_cast(inc, N) -> {noreply, N+1}.

handle_call({addStation,Name, Coords},_From, M) ->
  N = pollution:addStationToMonitor(Name, Coords, M),
  {reply, N, N}.

terminate(normal, N) -> io:format("Monitor: ~B~nBye.~n",[N]), ok.