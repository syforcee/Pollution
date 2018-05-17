%%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2018 15:02
%%%-------------------------------------------------------------------
-module(pollution).
-author("Pawel").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([addValue/5,removeValue/4,insertValue/5,createMonitor/0, addStationToMonitor/3, getOneValue/4, getStationMean/3, getDailyMean/3,getval/1,kickGradeint/2,gradient/3]).

%STRUCTURE
-record (measure, {date, type="", value=0}).
-record (station,{name="station", coordinates = {0,0}, measures=[]}).

%Creates new Monitor
createMonitor() -> [].

%Adds station to monitor, checks if no station with given name or coordinates already exist.
addStationToMonitor(Name,{A,B}, Monitor) ->
  case findStation(Name,Monitor) of
    true -> {error,"There already is a station with given name"};
    false -> case findStation({A,B},Monitor) of
               true -> {error,"There is already a station with given coordinates"};
               false -> [#station{name=Name, coordinates = {A,B}}|Monitor]
             end
  end.

checkIfExist(Date, Type, [#measure{date = Date, type = Type} | T])-> true;
checkIfExist(Date, Type,  [_| T]) ->
  checkIfExist(Date, Type, T);
checkIfExist(Date, Type,  []) -> false.

%Returns true/false, finds station (ADDITIONAL)
%findStation(Name, Monitor)
findStation(_, []) -> false;
findStation(Name, [#station{name=Name}|T]) -> true;
findStation(Coor, [#station{coordinates =Coor}|T]) -> true;
findStation(Name,[H|T]) -> findStation(Name,T).

%Adds measure
addValue(Station, Date, Type, Value, Monitor) ->
  case findStation(Station,Monitor) of
    true -> insertValue(Station, Date, Type, Value, Monitor);
    false -> {error,"There is no such station"}
  end.

%Helps adding
insertValue(_, _, _, _, []) -> [];
insertValue(Station, Data, Type, Value, [#station{name=Station, measures = L}=S | T]) ->
  %"found station";
  case checkIfExist(Data, Type, L) of
    true->{error,"That measure already exists"};
    false-> [S#station{measures = [ #measure{date = Data, type = Type, value = Value}|L]}|T]
  end;
insertValue(Station, Data, Type, Value, [H | T]) ->
  [H]++insertValue(Station, Data, Type, Value, T).



removeValue(Station, Date, Type, Monitor)->
  case findStation(Station,Monitor) of
    true -> doRemove(Station, Date, Type, Monitor);
    false -> {error,"Station does not exist"}
  end.

doRemove(_,_,_,[]) -> [];
doRemove(Station, Date, Type, [#station{name = Station, measures = L}=S|T])->
  case checkIfExist(Date, Type,L) of
    false->{error,"Measure does not exist"};
    true-> [S#station{measures = remove(Date, Type, L)} | T ]
  end;
doRemove(Station, Date, Type, [#station{coordinates = Station, measures = L}=S|T])->
  case checkIfExist(Date, Type,L) of
    false->{error,"Measure does not exist"};
    true ->[S#station{measures = remove(Date, Type, L)} | T ]
  end;
doRemove(Station,Date,Type, [H|T])-> [H|doRemove(Station,Date,Type,T)].

remove(Date,Type,[#measure{date = Date,type = Type}|T]) ->T;
remove(Date,Type,[H|T])-> [H]++remove(Date,Type,T);
remove(_,_,[])->[].

%Stats-------------------------------------------------------------------------------------------------

%Returns one particular value from seted station.(Type, Date, Station, Monitor)
getOneValue(_,_,_,[])-> {error,"No such station"};
getOneValue(Type, Date, Station, [#station{name = Station, measures = L}| T ]) -> getValueFromStation(Type,Date,L);
getOneValue(Type, Date, Station, [H|T]) -> getOneValue(Type,Date, Station, T).

getValueFromStation(_, _, [])-> {error, "Measure does not exist"};
getValueFromStation(Type, Date, [#measure{type = Type, date = Date}=M|T]) -> M#measure.value;
getValueFromStation(Type, Date, [H|T]) -> getValueFromStation(Type, Date, T).

%Returns avg value of goven parameter in given station
getStationMean(_,_,[])-> {error, "No such station"};
getStationMean(Type, Station, [#station{name=Station, measures = L}|T]) -> getAverage(Type,L,0,0);
getStationMean(Type,Station,[_|T]) -> getStationMean(Type, Station,T).

getAverage(_,[],_,0)->0;
getAverage(_,[],Sum,Len)->Sum/Len;
getAverage(Type, [#measure{type = Type, value = V}|T], Sum, Len) -> getAverage(Type, T, Sum+V,Len+1);
getAverage(Type, [_|T], Sum, Len) -> getAverage(Type, T, Sum,Len).

%Returns avg of given paramter from all stations on certain day
getDailyMean(Type,{Year, Month, Day}, M) -> avgPerMonitor(Type,{Year, Month, Day}, {0,0},M);
getDailyMean(Type, _ ,M) -> {error, "Wrong date format {YY,MM,DD} expected"}.

avgPerMonitor(_,_,{0,0},[])->{error,"No measures of given type was found"};
avgPerMonitor(_,_,{Sum,Len},[])->Sum/Len;
avgPerMonitor(Type, {Year, Month, Day}, {Sum,Len}, [#station{measures = L}|T]) ->avgPerMonitor(Type,{Year, Month, Day}, avgPerStation(Type,{Year, Month, Day},L,Sum,Len),T ).

avgPerStation(_,_,[],Sum,Len)->{Sum,Len};
avgPerStation(Type,{Year, Month, Day}, [#measure{type = Type, date = {{Year, Month, Day}, {_, _, _}}, value = V }|T],Sum,Len)-> avgPerStation(Type,{Year, Month, Day}, T,Sum+V,Len+1);
avgPerStation(Type,{Year, Month, Day}, [H |T],Sum,Len) ->  avgPerStation(Type,{Year, Month, Day}, T,Sum,Len).

%Gradient
getval([#station{measures = M}|T])->valueOfPollution("PM",M,0,0).

valueOfPollution(_,[],_,0)->0;
valueOfPollution(_,[],Sum,Len)->Sum/Len;
valueOfPollution(Type, [#measure{type = Type, value = V }|T],Sum,Len)-> valueOfPollution(Type, T,Sum+V,Len+1).

getDistance({A,B},{C,D})-> math:sqrt( (C-A)*(C-A) + (B-D)*(B-D) ).

kickGradeint([H|T],Type)-> gradient2(H,T,0,H,Type).

gradient([],_,List)->List;
gradient([H|T],Type,List) -> gradient(T,Type,List++[gradient2(H,T,0,H,Type)]).

gradient2(S,[],_,S,_)->{error,"same input output station"};
gradient2(S,[],Max,S2,_)->{S,S2,Max};
gradient2(S, [#station{coordinates = {A,B}, measures = M}=S2|T], Max, SB,Type)->
  case S#station.coordinates == S2#station.coordinates of
    false-> case ( ((valueOfPollution(Type,S#station.measures,0,0)) / getDistance(S#station.coordinates,S2#station.coordinates)) )>Max  of
              true-> gradient2(S, T,((valueOfPollution(Type,S#station.measures,0,0)) / getDistance(S#station.coordinates,S2#station.coordinates)),S2,Type);
              false->gradient2(S, T, Max, SB,Type)
            end;
    true-> gradient2(S, T, Max, SB,Type)
  end.