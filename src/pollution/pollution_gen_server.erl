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
-export([start_link/0, init/1, handle_call/3, terminate/2, handle_cast/2, stop/0]).
-export([addStation/2, addValue/4, removeValue/3, getDailyMean/2, getStationMean/2, getOneValue/3]).

%% START %%
start_link()   ->
  M = pollution:createMonitor(),
  gen_server:start_link({local,?MODULE},?MODULE,M,[]).

%%INIT%%
init(N)-> {ok,N}.

%%CAST FUNCTIONS%%
addStation(Name, Coords) -> gen_server:cast(?MODULE, {addStation, Name, Coords}).
addValue(Station, Date, Type, Value) -> gen_server:cast(?MODULE, {addValue, Station, Date, Type, Value}).
removeValue(Station, Date, Type) -> gen_server:cast(?MODULE, {removeValue, Station, Date, Type}).

%%CALL FUNCTIONS%%
getOneValue(Station, Date, Type) -> gen_server:call(?MODULE, {getOneValue, Station, Date, Type}).
getStationMean(Type, Station) -> gen_server:call(?MODULE, {getStationMean, Type, Station}).
getDailyMean(Type, Date) -> gen_server:call(?MODULE, {getDailyMean, Type, Date}).

stop() ->
  gen_server:call(?MODULE, terminate).
%%gradient TO DO%%

%% MESSAGE HANDLERS %%
handle_cast({addStation,Name, Coords}, M) ->
  N = controlResult(M, pollution:addStationToMonitor(Name, Coords, M)),
  {noreply, N};
handle_cast({addValue, Station, Date, Type, Value}, M)->
  N = controlResult(M, pollution:addValue(Station, Date, Type, Value, M)),
  {noreply, N};
handle_cast({removeValue, Station, Date, Type}, M)->
  N = controlResult(M, pollution:removeValue(Station, Date, Type, M)),
  {noreply, N}.

handle_call(terminate, _From, M) ->
  {stop, normal, ok, M};

handle_call({getOneValue, Station, Date, Type},_From,  M) ->
  N = controlResponse(M, pollution:getOneValue(Type, Date, Station, M)),
  {reply, N, M};

handle_call({getStationMean, Type, Station},_From, M)->
  N = controlResponse(M, pollution:getStationMean(Type, Station, M)),
  {reply, N, M};

handle_call({getDailyMean, Type, Date},_From, M) ->
  N = controlResponse(M, pollution:getDailyMean(Type, Date, M)),
  {reply, N, M}.

%%CONTROL ERROR FUNCTION%%

%%cast chceck%%
controlResult(M, {error, Description}) ->
  io:format("error: ~s~n", [Description]),
  M;
controlResult(_, N)->
  io:format("~w~n", [N]),
  N.

%%call chceck%%
controlResponse(M, {error, Description}) ->
  io:format("error: ~s~n", [Description]),
  M;
controlResponse(M, N)->
  io:format("~w~n", [N]),
  M.

%%TERMINATE
terminate(normal, N) -> io:format("Monitor: ~w~n Bye.~n",[N]), ok.