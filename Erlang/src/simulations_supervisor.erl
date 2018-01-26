%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:03
%%%-------------------------------------------------------------------
-module(simulations_supervisor).
-author("motek").

-behaviour(supervisor).

%% API
-export([start_link/1,start_simulation/1,stop_simulation/0,generate_cars/2,generate_pedestrians/2]).
-include("../include/records.hrl").
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_simulation(WorldParameters) ->
  start_lights(WorldParameters),
  simulation_traffic_supervisor:generate_cars(WorldParameters,1),
  done.
stop_simulation() ->
  stop_lights(),
  simulation_pedestrians_supervisor:kill_children(),
  simulation_traffic_supervisor:kill_children(),
  done.
generate_pedestrians(WorldParameters,Amount) ->
  simulation_pedestrians_supervisor:generate_pedestrians(WorldParameters,Amount).
generate_cars(WorldParameters,Amount) ->
  simulation_traffic_supervisor:generate_cars(WorldParameters,Amount).

start_link(WorldParameters) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [WorldParameters]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init(WorldParameters) ->
  Args = [WorldParameters],

  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = brutal_kill,
  Type = supervisor,

  UUIDProvider = {
    uuid_provider,
    {uuid_provider, start_link, []},
    Restart, Shutdown, worker,
    [ uuid_provider ]
  },
  TrafficSupervisor = {
    traffic_supervisor,
    {simulation_traffic_supervisor, start_link, Args},
    Restart, Shutdown, Type,
    [ simulation_traffic_supervisor ]
  },
  PedestriansSupervisor = {
    simulation_pedestrians_supervisor,
    {simulation_pedestrians_supervisor, start_link, Args},
    Restart, Shutdown, Type,
    [ simulation_pedestrians_supervisor ]
  },
  Light = { light_entity,
    {light_entity, start_link, Args},
    temporary, brutal_kill, worker,
    [ light_entity ]},

  {ok, {SupFlags, [UUIDProvider, PedestriansSupervisor, TrafficSupervisor, Light]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_lights(_WorldParameters) ->
  gen_statem:call(light_entity,start).

stop_lights() -> common_defs:stop_children(?MODULE).