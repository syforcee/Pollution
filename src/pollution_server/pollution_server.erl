%%%-------------------------------------------------------------------
%%% @author Pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2018 20:50
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Pawel").

%% API
-export([start/0,stop/0,crash/0,init/0]).
%%-export([addStation/2]).
-export([addValue/4,addStation/2,getStationMean/2,getDailyMean/2,getOneValue/3, removeValue/3]).

start() ->
  register(pollutionServer, spawn_link(pollution_server, init,[])).

stop() ->
  pollutionServer ! stop.

init() ->
  %%io:format("Server started~n"),
  loop(pollution:createMonitor()).

crash() ->
  io:format("crashing..~n"),
  pollutionServer ! crash.

loop(Monitor) ->
  receive
    {Pid, addStation, {Name, {A,B}}} ->
      M = pollution:addStationToMonitor(Name,{A,B}, Monitor),
      loop(checkResult(Monitor, M, Pid));
    {Pid, addValue, {Station, Date, Type, Value }}->
      M = pollution:addValue(Station, Date, Type, Value, Monitor),
      loop(checkResult(Monitor, M, Pid));
    {Pid, removeValue, {Station, Date, Type}} ->
      M = pollution:removeValue(Station, Date, Type, Monitor),
      loop(checkResult(Monitor, M, Pid));

    {Pid, getOneValue, {Station, Date, Type}} ->
      M = pollution:getOneValue(Type, Date, Station, Monitor),
      loop(controlGetting(Monitor, M, Pid));
    {Pid, getDailyMean, {Type, Date}} ->
      M = pollution:getDailyMean(Type, Date, Monitor),
      loop(controlGetting(Monitor, M, Pid));
    {Pid, getStationMean, {Type, Station}} ->
      M = pollution:getStationMean(Type, Station, Monitor),
      loop(controlGetting(Monitor, M, Pid));
    stop -> stop();
    crash -> 1/0;
    _ -> io:format("Received unknown command"),
        loop(Monitor)
  end.

checkResult(Monitor, {error, Msg}, Pid)->
  io:format("Error: ~s ~n",[Msg]),
  Pid ! Monitor,
  Monitor;
checkResult(_, NewMonitor, Pid) ->
  Pid ! NewMonitor,
  NewMonitor.

controlGetting(Monitor, {error, Msg}, Pid) ->
  Pid ! {error, Msg},
  Monitor;
controlGetting(Monitor, V, Pid) ->
  Pid ! Monitor,
  Monitor.

request(Msg, Args)->
  pollutionServer ! {self(), Msg, Args},
  receive
    Reply-> Reply
  end.

%API functions
addStation(Name, Coords) -> request(addStation, {Name, Coords}).
addValue(Station, Date,Type,Value) -> request(addValue, {Station, Date, Type, Value}).
removeValue(Station, Date, Type) -> request(removeValue, {Station, Date, Type}).
getOneValue(Station, Date, Type) -> request(getOneValue, {Station, Date, Type}).
getStationMean(Station, Type) -> request(getStationMean, {Station, Type}).
getDailyMean(Date, Type) -> request(getDailyMean, {Date, Type}).