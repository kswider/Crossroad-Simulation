%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:04
%%%-------------------------------------------------------------------
-module(simulation_traffic_supervisor).
-author("motek").

-behaviour(supervisor).

-include("../include/records.hrl").

%% API
-export([start_link/1,generate_cars/2,kill_children/0,spawn_car/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
generate_cars(_WorldParameters,0) -> done;
generate_cars(WorldParameters,Amount) ->
  spawn(?MODULE,spawn_car,[WorldParameters, gen_server:call(uuid_provider,next_car), common_defs:get_random(car,WorldParameters)]),
  generate_cars(WorldParameters,Amount-1).

kill_children() -> common_defs:stop_children(?MODULE).

start_link(WorldParameters) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, WorldParameters).

spawn_car(WorldParameters,UUID,CarParams) ->
  timer:sleep(rand:uniform(400)),
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  {Pos,_Dest} = CarParams,
  case common_defs:ask_cars_for_position(Cars,Pos#position.x,Pos#position.y,self()) of
    free ->
      Car = { {car, UUID},
        {car_entity, start_link, [{WorldParameters,CarParams}]},
        temporary, brutal_kill, worker,
        [ car_entity ]},
      supervisor:start_child(?MODULE, Car);
    _ ->
      timer:sleep(1000),
      spawn_car(WorldParameters,UUID,CarParams)
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_WorldParameters) ->
  simulation_event_stream:component_ready(?MODULE),

  RestartStrategy = one_for_one,
  MaxRestarts = 200,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


