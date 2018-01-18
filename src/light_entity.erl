%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2018 20:35
%%%-------------------------------------------------------------------
-module(light_entity).
-author("motek").

-behaviour(gen_statem).

-include("../include/records.hrl").
%% API
-export([start_link/1]).
-export([red/3,green/3,yellow/3,not_started/3]).

%% gen_statem callbacks
-export([
  init/1,
  handle_event/3,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).

start_link([WorldParameters]) ->
  gen_statem:start_link({local,?SERVER}, ?MODULE, WorldParameters, []).

init(WorldParameters) ->
  Data = #light{pid = self,main_road = horizontal, world_parameters = WorldParameters},
  {ok,not_started,Data}.

%TODO: ERROR IS HERE
not_started({call, From}, start, Data) ->
  {next_state, green, Data, [{reply,From,started},{state_timeout,Data#light.world_parameters#world_parameters.yellow_light_time,change_time}]}.
green(state_timeout,change_time,Data) ->
  case is_someone_on_sub_road(Data) of
    true ->
      simulation_event_stream:notify(lights,changes_to_yellow,Data),
      {next_state, yellow, Data, [{state_timeout,Data#light.world_parameters#world_parameters.yellow_light_time,change_to_red_time}]};
    false ->
      {keep_state,Data, [{state_timeout,Data#light.world_parameters#world_parameters.main_light_time,change_time}]}
  end;
green({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,green}]};
green({call,From}, get_sub_road_lights, Data) ->
  {keep_state,Data,[{reply,From,red}]}.

yellow(state_timeout,change_to_red_time,Data) ->
  simulation_event_stream:notify(lights,changes_to_red,Data),
  {next_state, red, Data, [{state_timeout,Data#light.world_parameters#world_parameters.sub_light_time,change_time}]};
yellow(state_timeout,change_to_green_time,Data) ->
  simulation_event_stream:notify(lights,changes_to_green,Data),
  {next_state, green, Data, [{state_timeout,Data#light.world_parameters#world_parameters.main_light_time,change_time}]};
yellow({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,yellow}]};
yellow({call,From}, get_sub_road_lights, Data) ->
  {keep_state,Data,[{reply,From,yellow}]}.

red(state_timeout,change_time,Data) ->
  case is_someone_on_sub_road(Data) and not is_someone_on_main_road(Data) of
    true ->
      {keep_state,Data, [{state_timeout,Data#light.world_parameters#world_parameters.sub_light_time,change_time}]};
    false ->
      simulation_event_stream:notify(lights,changes_to_yellow,Data),
      {next_state, yellow, Data, [{state_timeout,Data#light.world_parameters#world_parameters.yellow_light_time,change_to_green_time}]}
  end;
red({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,red}]};
red({call,From}, get_sub_road_lights, Data) ->
  {keep_state,Data,[{reply,From,green}]}.


handle_event(_, _, Data) ->
  %% Ignore all other events
  {keep_state,Data}.

is_someone_on_main_road(Data) ->
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  Pedestrians = supervisor:which_children(simulation_pedestrians_supervisor),
  PedestrianWaiting = are_pedestrian_waiting(Pedestrians,common_defs:get_pedestrians_sub_road_enter_crossing_points(Data#light.world_parameters)),
  CarsWaiting = are_cars_waiting(Cars,common_defs:get_cars_main_enter_crossing_points(Data#light.world_parameters)),
  PedestrianWaiting and CarsWaiting.

are_pedestrian_waiting(_Pedestrian,[]) ->
  false;
are_pedestrian_waiting(Pedestrians,[{X,Y,_,_}|PosTail]) ->
  case common_defs:ask_pedestrians_for_position(Pedestrians,X,Y) of
    free -> are_pedestrian_waiting(Pedestrians,PosTail);
    _ -> true
  end.

are_cars_waiting(_Car,[]) ->
  false;
are_cars_waiting(Cars,[{X,Y,_,_}|PosTail]) ->
  case common_defs:ask_pedestrians_for_position(Cars,X,Y) of
    free -> are_cars_waiting(Cars,PosTail);
    _ -> true
  end.

is_someone_on_sub_road(Data) ->
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  Pedestrians = supervisor:which_children(simulation_pedestrians_supervisor),
  PedestrianWaiting = are_pedestrian_waiting(Pedestrians,common_defs:get_pedestrians_sub_road_enter_crossing_points(Data#light.world_parameters)),
  CarsWaiting = are_cars_waiting(Cars,common_defs:get_cars_sub_road_enter_crossing_points(Data#light.world_parameters)),
  PedestrianWaiting and CarsWaiting.


terminate(_Reason, _State, _Data) ->
  void.
code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.
callback_mode() -> state_functions.