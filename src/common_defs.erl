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
-export([stop_children/1, random_directions/1, random_destination/0, random_direction/0, random_pedestrian_start_position/1, get_pedestrian_turn_points/1, get_pedestrian_enter_crossing_points/1, get_pedestrians_start_points/1, should_dissapear/2, get_cars_start_points/1, get_cars_enter_crossing_points/1, get_random_car_start_point/1, get_cars_turn_points/1, ask_cars_for_position/3, ask_pedestrians_for_position/3, get_cars_main_enter_crossing_points/1, get_cars_sub_road_enter_crossing_points/1, get_pedestrians_main_road_enter_crossing_points/1, get_pedestrians_sub_road_enter_crossing_points/1]).

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

random_pedestrian_start_position(WorldParameters) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  {X,Y,LookX,LookY} = lists:nth(random:uniform(8),get_pedestrians_start_points(WorldParameters)),
  #position{
    x = X,
    y = Y,
    look_x = LookX,
    look_y = LookY
  }.

get_pedestrians_start_points(WorldParameters) ->
  Y = WorldParameters#world_parameters.world_height - 1,
  X = WorldParameters#world_parameters.world_width - 1,
  {X1,X2} = case X rem 2 of
              0 -> {X div 2-3,X div 2+3};
              1 -> {X div 2-3,X div 2+4}
            end,
  {Y1,Y2} = case Y rem 2 of
              0 -> {Y div 2-3,Y div 2+3};
              1 -> {Y div 2-3,Y div 2+4}
            end,
  [{X1,0,0,1},{X1,Y,0,-1},{X2,0,0,1},{X2,Y,0,-1},{0,Y1,1,0},{X,Y1,-1,0},{0,Y2,1,0},{X,Y2,-1,0}].

get_random_car_start_point(WorldParameters) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  {X,Y,LookX,LookY} = lists:nth(random:uniform(4),get_pedestrians_start_points(WorldParameters)),
  #position{
    x = X,
    y = Y,
    look_x = LookX,
    look_y = LookY
  }.

get_pedestrian_turn_points(WorldParameters) ->
  Y = WorldParameters#world_parameters.world_height - 1,
  X = WorldParameters#world_parameters.world_width - 1,
  {X1,X2} = case X rem 2 of
              0 -> {X div 2-3,X div 2+3};
              1 -> {X div 2-3,X div 2+4}
            end,
  {Y1,Y2} = case Y rem 2 of
              0 -> {Y div 2-3,Y div 2+3};
              1 -> {Y div 2-3,Y div 2+4}
            end,
  [{X1,Y1},{X1,Y2},{X2,Y1},{X2,Y2}].

get_pedestrian_enter_crossing_points(WorldParameters) ->
  Y = WorldParameters#world_parameters.world_height - 1,
  X = WorldParameters#world_parameters.world_width - 1,
  {X1,X2} = case X  rem  2 of
              0 -> {X rem 2-3,X rem 2+3};
              1 -> {X rem 2-3,X rem 2+4}
            end,
  {Y1,Y2} = case Y  rem  2 of
              0 -> {Y rem 2-3,Y rem 2+3};
              1 -> {Y rem 2-3,Y rem 2+4}
            end,
  [{X1+1,Y1,1,0},{X1+1,Y2,1,0},{X2-1,Y1,-1,0},{X2-1,Y2,-1,0},{X1,Y1+1,0,1},{X1,Y2-1,0,-1},{X2,Y1+1,0,1},{X2,Y2-1,0,-1}].

get_cars_start_points(WorldParameters) ->
  Y = WorldParameters#world_parameters.world_height - 1,
  X = WorldParameters#world_parameters.world_width - 1,
  {X1,X2} = case X rem 2 of
              0 -> {X div 2-1,X div 2+1};
              1 -> {X div 2-1,X div 2+2}
            end,
  {Y1,Y2} = case Y rem 2 of
              0 -> {Y div 2-1,Y div 2+1};
              1 -> {Y div 2-1,Y div 2+2}
            end,
  [{0,Y1,1,0},{X,Y2,-1,0},{X1,0,0,1},{X2,Y,0,-1}].

get_cars_enter_crossing_points(WorldParameters) -> todo ,[].
get_pedestrians_sub_road_enter_crossing_points(WorldParameters) -> todo, [].
get_pedestrians_main_road_enter_crossing_points(WorldParameters) -> todo, [].
get_cars_sub_road_enter_crossing_points(WorldParameters) -> todo, [].
get_cars_main_enter_crossing_points(WorldParameters) -> todo, [].
get_cars_turn_points(WorldParameters) -> todo, [].

should_dissapear(WorldParameters,Position) ->
  (Position#position.x < 0) or (Position#position.x > WorldParameters#world_parameters.world_width-1) or
    (Position#position.y < 0) or (Position#position.y > WorldParameters#world_parameters.world_height-1).

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