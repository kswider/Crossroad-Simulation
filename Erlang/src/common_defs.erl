%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:10
%%%-------------------------------------------------------------------
-module(common_defs).

-author("motek").

-include("../include/records.hrl").

%% API
-export([get_start_points/2,get_turn_points/2,get_waiting_points/2,get_random/2,get_waiting_points/3]).
-export([stop_children/1,ask_pedestrians_for_position/3,ask_cars_for_position/4,should_dissapear/2,
  ask_cars_for_position_2/4, random_directions/1]).

stop_children(SupervisorName) ->
  [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].

random_direction() ->
  case rand:uniform(3) of
    1 -> forward;
    2 -> left;
    3 -> right
  end.

random_directions(N) ->
  random_directions(N,[]).
random_directions(0,List) ->
  List;
random_directions(N,List) ->
  random_directions(N-1,[random_direction()|List]).

random_destination() -> random_direction().

get_start_points(pedestrian,_WorldParameters) ->
  [
    #position{x = 4, y = 0, look_x = 0, look_y = 1},
    #position{x = 11, y = 0, look_x = 0, look_y = 1},
    #position{x = 15, y = 4, look_x = -1, look_y = 0},
    #position{x = 15, y = 11, look_x = -1, look_y = 0},
    #position{x = 11, y = 15, look_x = 0, look_y = -1},
    #position{x = 4, y = 15, look_x = 0, look_y = -1},
    #position{x = 0, y = 11, look_x = 1, look_y = 0},
    #position{x = 0, y = 4, look_x = 1, look_y = 0}
  ];
get_start_points(car,_WorldParameters) ->
  [
    #position{x = 8, y = -1, look_x = 0, look_y = 1},
    #position{x = 16, y = 8, look_x = -1, look_y = 0},
    #position{x = 7, y = 16, look_x = 0, look_y = -1},
    #position{x = -1, y = 7, look_x = 1, look_y = 0}
  ].

get_waiting_points(Who,WorldParameters) ->
  lists:append(get_waiting_points(Who,main_road,WorldParameters), get_waiting_points(Who,sub_road,WorldParameters)).

get_waiting_points(car,main_road,_WorldParameters) ->
  [
    #position{x = 8, y = 3, look_x = 0, look_y = 1},
    #position{x = 7, y = 12, look_x = 0, look_y = -1}
  ];
get_waiting_points(car,sub_road,_WorldParameters) ->
  [
    #position{x = 3, y = 7, look_x = 1, look_y = 0},
    #position{x = 12, y = 8, look_x = -1, look_y = 0}
  ];
get_waiting_points(pedestrian,sub_road,_WorldParameters) ->
  [
    #position{x = 6, y = 4, look_x = 1, look_y = 0},
    #position{x = 9, y = 4, look_x = -1, look_y = 0},
    #position{x = 9, y = 11, look_x = -1, look_y = 0},
    #position{x = 6, y = 11, look_x = 1, look_y = 0}
  ];
get_waiting_points(pedestrian,main_road,_WorldParameters) ->
  [
    #position{x = 11, y = 6, look_x = 0, look_y = 1},
    #position{x = 11, y = 9, look_x = 0, look_y = -1},
    #position{x = 4, y = 9, look_x = 0, look_y = -1},
    #position{x = 4, y = 6, look_x = 0, look_y = 1}
  ].

get_turn_points(car,_WorldParameters) ->
  [
    #position{x = 8, y = 8, look_x = 0, look_y = 0},
    #position{x = 7, y = 7, look_x = 0, look_y = 0},
    #position{x = 8, y = 7, look_x = 0, look_y = 0},
    #position{x = 7, y = 8, look_x = 0, look_y = 0}
  ];

get_turn_points(pedestrian,_WorldParameters) ->
  [
    #position{x = 4, y = 4, look_x = 0, look_y = 0},
    #position{x = 4, y = 11, look_x = 0, look_y = 0},
    #position{x = 11, y = 11, look_x = 0, look_y = 0},
    #position{x = 11, y = 4, look_x = 0, look_y = 0}
  ].

get_random(pedestrian,WorldParameters) ->
  Pos = lists:nth(rand:uniform(8),get_start_points(pedestrian,WorldParameters)),
  Direct = random_directions(4),
  {Pos,Direct};
get_random(car,WorldParameters) ->
  Pos = lists:nth(rand:uniform(4),get_start_points(car,WorldParameters)),
  Dest = random_destination(),
  {Pos,Dest}.

should_dissapear(_WorldParameters,Position) ->
  (Position#position.x < -1) or (Position#position.x > 16) or
    (Position#position.y < -1) or (Position#position.y > 16).

ask_cars_for_position([], _NxtPositionPositionX, _NxtPositionPositionY, _MyPid) -> free;
ask_cars_for_position([ {_Id, MyPid, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY, MyPid) ->
  ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY,MyPid);
ask_cars_for_position([ {_Id, Car, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY, MyPid) ->
  try gen_server:call(Car, {are_you_at, NxtPositionPositionX, NxtPositionPositionY},300) of
    true ->
      Car;
    false ->
      ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY,MyPid)
  catch
    exit:_Reason ->
      timeout
  end.

ask_cars_for_position_2([], _NxtPositionPositionX, _NxtPositionPositionY, _MyPid) -> free;
ask_cars_for_position_2([ {_Id, MyPid, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY, MyPid) ->
  ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY,MyPid);
ask_cars_for_position_2([ {_Id, Car, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY, MyPid) ->
  try gen_server:call(Car, {will_you_be_at, NxtPositionPositionX, NxtPositionPositionY},300) of
    true ->
      Car;
    false ->
      ask_cars_for_position_2(Rest,NxtPositionPositionX,NxtPositionPositionY,MyPid)
  catch
    exit:_Reason ->
      timeout
  end.

ask_pedestrians_for_position([], _NxtPositionPositionX, _NxtPositionPositionY) -> free;
ask_pedestrians_for_position([ {_Id, Pedestrian, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY) ->
  gen_server:call(Pedestrian, {are_you_at, NxtPositionPositionX, NxtPositionPositionY}),
  try gen_server:call(Pedestrian, {are_you_at, NxtPositionPositionX, NxtPositionPositionY}) of
    true ->
      not_free;
    false ->
      ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY,self())
  catch
    exit:_Reason ->
      timeout
  end.