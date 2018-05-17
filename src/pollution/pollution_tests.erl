%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2018 00:10
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("pawel").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

createMonitor_test() ->
  ?assertEqual('pollution':createMonitor(), []).

addStationError_test() ->
  A = 'pollution':createMonitor(),
  ?assertNotMatch({error, _}, 'pollution':addStationToMonitor("a", {2, 3}, A)).

addStation_test() ->
  A = 'pollution':createMonitor(),
  ?assertEqual('pollution':addStationToMonitor("a", {2, 3}, A), [{station, "a", {2, 3}, []}]).

addValue_test() ->
  A = 'pollution':createMonitor(),
  B = 'pollution':addStationToMonitor("a", {2, 3}, A),
  ?assertEqual('pollution':addValue("a","2000","PM",1,B),[{station,"a",{2,3},[{measure,"2000","PM",1}]}]).

remove_test()->
  A = 'pollution':createMonitor(),
  B = 'pollution':addStationToMonitor("a", {2, 3}, A),
  C = 'pollution':addValue("a","2000","PM",1,B),
  ?assertEqual('pollution':removeValue("a","2000","PM",C), [{station, "a", {2, 3}, []}]).

gradient_test()->
  P='pollution':createMonitor(),
  P2='pollution':addStationToMonitor("Krk",{1,1},P),
  P3='pollution':addStationToMonitor("Mie",{1,22},P2),
  P4='pollution':addStationToMonitor("Poz",{1,5},P3),
  P5='pollution':addValue("Krk",{{2001,01,01},{01,01,1}},"PM",1,P4),
  P6='pollution':addValue("Mie",{{2001,01,01},{01,01,1}},"PM",1,P5),
  P7='pollution':addValue("Poz",{{2001,01,01},{01,01,1}},"PM",100000,P6),
  ?assertEqual('pollution':gradient(P7,"PM",[]), [{{station,"Poz",
    {1,5},
    [{measure,{{2001,1,1},{1,1,1}},"PM",100000}]},
    {station,"Krk",
      {1,1},
      [{measure,{{2001,1,1},{1,1,1}},"PM",1}]},
    2.5e4},
    {{station,"Mie",
      {1,22},
      [{measure,{{2001,1,1},{1,1,1}},"PM",1}]},
      {station,"Krk",
        {1,1},
        [{measure,{{2001,1,1},{1,1,1}},"PM",1}]},
      0.047619047619047616},
    {error,"same input output station"}] ).

getStationMean_test() ->
  P='pollution':createMonitor(),
  P2='pollution':addStationToMonitor("Krk",{1,1},P),
  P3='pollution':addValue("Krk",{{2001,01,01},{01,01,1}},"PM",1,P2),
  ?assertEqual('pollution':getStationMean("PM", "Krk"), 1.0).

%%getDailyMean_test() ->
%%  P='pollution':createMonitor(),
%%  P2='pollution':addStationToMonitor("Krk",{1,1},P),
%%  P3='pollution':addValue("Krk",{{2001,01,01},{01,01,1}},"PM",1,P2),
%%  ?assertEqual('pollution':getDaMean("PM", "Krk"), 1.0).