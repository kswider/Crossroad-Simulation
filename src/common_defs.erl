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
-export([stop_children/1,random_directions/1,random_direction/0,random_pedestrian_start_position/0]).

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

random_pedestrian_start_position() ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  {X,Y,LookX,LookY} = lists:nth(random:uniform(8),?PEDESTRIAN_START_POSITIONS),
  #position{
    x = X,
    y = Y,
    look_x = LookX,
    look_y = LookY
  }.