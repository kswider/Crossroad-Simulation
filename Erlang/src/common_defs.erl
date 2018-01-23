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
-export([get_start_points/2,get_turn_points/2,get_waiting_points/2,get_random/2]).
-export([stop_children/1,ask_pedestrians_for_position/3,ask_cars_for_position/3,should_dissapear/2]).

stop_children(SupervisorName) ->
  [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].

random_direction() ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  case random:uniform(3) of
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
    #position{x = 15, y = 10, look_x = -1, look_y = 0},
    #position{x = 11, y = 15, look_x = 0, look_y = -1},
    #position{x = 4, y = 15, look_x = 0, look_y = -1},
    #position{x = 0, y = 11, look_x = 1, look_y = 0},
    #position{x = 0, y = 4, look_x = 1, look_y = 0}
  ];
get_start_points(car,_WorldParameters) ->
  [
    #position{x = 8, y = 0, look_x = 0, look_y = 1},
    #position{x = 15, y = 8, look_x = -1, look_y = 0},
    #position{x = 7, y = 15, look_x = 0, look_y = -1},
    #position{x = 0, y = 7, look_x = 1, look_y = 0}
  ].

get_waiting_points(pedestrian,_WorldParameters) ->
  [
    #position{x = 6, y = 4, look_x = 1, look_y = 0},
    #position{x = 9, y = 0, look_x = -1, look_y = 0},
    #position{x = 11, y = 6, look_x = 0, look_y = 1},
    #position{x = 11, y = 9, look_x = 0, look_y = -1},
    #position{x = 9, y = 11, look_x = -1, look_y = 0},
    #position{x = 6, y = 11, look_x = 1, look_y = 0},
    #position{x = 4, y = 9, look_x = 0, look_y = -1},
    #position{x = 4, y = 6, look_x = 0, look_y = 1}
  ];

get_waiting_points(car,_WorldParameters) ->
  [
    #position{x = 8, y = 3, look_x = 0, look_y = 1},
    #position{x = 7, y = 12, look_x = 0, look_y = -1},
    #position{x = 3, y = 7, look_x = 1, look_y = 0},
    #position{x = 12, y = 8, look_x = -1, look_y = 0}
  ].

get_turn_points(car,_WorldParameters) ->
  [
    #position{x = 8, y = 8, look_x = 0, look_y = 1},
    #position{x = 7, y = 7, look_x = 0, look_y = -1},
    #position{x = 8, y = 7, look_x = 1, look_y = 0},
    #position{x = 7, y = 8, look_x = -1, look_y = 0}
  ];

get_turn_points(pedestrian,_WorldParameters) ->
  [
    #position{x = 4, y = 4, look_x = 0, look_y = 0},
    #position{x = 4, y = 11, look_x = 0, look_y = 0},
    #position{x = 11, y = 11, look_x = 0, look_y = 0},
    #position{x = 11, y = 4, look_x = 0, look_y = 0}
  ].

get_random(pedestrian,WorldParameters) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  Pos = lists:nth(random:uniform(8),get_start_points(pedestrian,WorldParameters)),
  Direct = random_directions(4),
  {Pos,Direct};
get_random(car,WorldParameters) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  Pos = lists:nth(random:uniform(4),get_start_points(car,WorldParameters)),
  Dest = random_destination(),
  {Pos,Dest}.

should_dissapear(_WorldParameters,Position) ->
  (Position#position.x < 0) or (Position#position.x > 15) or
    (Position#position.y < 0) or (Position#position.y > 15).

ask_cars_for_position([], _NxtPositionPositionX, _NxtPositionPositionY) -> free;
ask_cars_for_position([ {_Id, Car, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY) ->
  try gen_server:call(Car, {are_you_at, NxtPositionPositionX, NxtPositionPositionY}) of
    true ->
      not_free;
    false ->
      ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY)
  catch
    exit:_Reason -> not_free
  end.

ask_pedestrians_for_position([], _NxtPositionPositionX, _NxtPositionPositionY) -> free;
ask_pedestrians_for_position([ {_Id, Pedestrian, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY) ->
  try gen_server:call(Pedestrian, {are_you_at, NxtPositionPositionX, NxtPositionPositionY}) of
    true ->
      not_free;
    false ->
      ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY)
  catch
    exit:_Reason -> not_free
  end.