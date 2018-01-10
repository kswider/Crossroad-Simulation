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

-record(state, {}).

start_link(WorldParameters) ->
  gen_statem:start_link({local,?SERVER}, ?MODULE, WorldParameters, []).

init(WorldParameters) ->
  Data = #light{pid = self,main_road = horizontal, waiting = [], world_parameters = WorldParameters},
  erlang:start_timer(100, self(), start),
  {ok,not_started,Data}.

not_started(info, {timeout,Tref,start}, Data) ->
  {next_state, green, Data, [{state_timeout,Data#world_parameters.yellow_light_time,change_time}]}.
green(state_timeout,change_time,Data) ->
  case is_someone_on_sub_road() of
    true ->
      simulation_event_stream:notify(lights,changes_to_yellow,Data),
      {next_state, yellow, Data, [{state_timeout,Data#world_parameters.yellow_light_time,change_to_red_time}]};
    false ->
      {keep_state,Data, [{state_timeout,Data#world_parameters.main_light_time,change_time}]}
  end;
green({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,green}]};
green({call,From}, get_sub_road_lights, Data) ->
  {keep_state,Data#{waiting := [From|Data#light.waiting]},[{reply,From,red}]}.

yellow(state_timeout,change_to_red_time,Data) ->
  notify_all_waiting(),
  simulation_event_stream:notify(lights,changes_to_red,Data),
  {next_state, red, Data, [{state_timeout,Data#world_parameters.sub_light_time,change_time}]};
yellow(state_timeout,change_to_green_time,Data) ->
  notify_all_waiting(),
  simulation_event_stream:notify(lights,changes_to_green,Data),
  {next_state, green, Data, [{state_timeout,Data#world_parameters.main_light_time,change_time}]};
yellow({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,yellow}]};
yellow({call,From}, get_sub_road_lights, Data) ->
  {keep_state,Data,[{reply,From,yellow}]}.

red(state_timeout,change_time,Data) ->
  case is_someone_on_sub_road() and not is_someone_on_main_road() of
    true ->
      {keep_state,Data, [{state_timeout,Data#world_parameters.sub_light_time,change_time}]};
    false ->
      simulation_event_stream:notify(lights,changes_to_yellow,Data),
      {next_state, yellow, Data, [{state_timeout,Data#world_parameters.yellow_light_time,change_to_green_time}]}
  end;
red({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data#{waiting := [From|Data#light.waiting]},[{reply,From,red}]};
red({call,From}, get_main_road_lights, Data) ->
  {keep_state,Data,[{reply,From,green}]}.

handle_event(_, _, Data) ->
  %% Ignore all other events
  {keep_state,Data}.

%TODO: implement this
is_someone_on_sub_road() -> true.
is_someone_on_main_road() -> true.
notify_all_waiting() -> done.


terminate(_Reason, _State, _Data) ->
  void.
code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.
callback_mode() -> state_functions.