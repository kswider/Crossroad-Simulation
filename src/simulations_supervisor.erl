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
-export([start_link/1,start_simulation/1,stop_simulation/0,generate_cars/1,generate_pedestrians/1]).
-include("../include/records.hrl").
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WorldParameters) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [WorldParameters]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init(WorldParameters) ->
  Args = [WorldParameters],

  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = brutal_kill,
  Type = supervisor,

  TrafficSupervisor = {
    traffic_supervisor,
    {simulation_traffic_supervisor, start_link, Args},
    Restart, Shutdown, Type,
    [ simulation_traffic_supervisor ]
  },
  PedestriansSupervisor = {
    pedestrians_supervisor,
    {simulation_pedestrians_supervisor, start_link, Args},
    Restart, Shutdown, Type,
    [ simulation_pedestrians_supervisor ]
  },
  LightsSupervisor = {
    lights_supervisor,
    {simulation_lights_supervisor, start_link, Args},
    Restart, Shutdown, Type,
    [ simulation_lights_supervisor ]
  },

  {ok, {SupFlags, [TrafficSupervisor,PedestriansSupervisor,LightsSupervisor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_simulation(WorldParameters) -> 3.
stop_simulation() -> 3.
generate_pedestrians(Amount) -> 3.
generate_cars(Amount) -> 3.