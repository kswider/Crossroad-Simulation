%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:00
%%%-------------------------------------------------------------------
-module(simulation_main_supervisor).
-author("motek").

-behaviour(supervisor).
-include("../include/records.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-export([start_simulation/1,stop_simulation/0,generate_cars/2,generate_pedestrians/2]).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_simulation(WorldParameters) -> simulations_supervisor:start_simulation(WorldParameters).
stop_simulation() -> simulations_supervisor:stop_simulation().
generate_pedestrians(WorldParameters,Amount) -> simulations_supervisor:generate_pedestrians(WorldParameters,Amount).
generate_cars(WorldParameters,Amount) -> simulations_supervisor:generate_cars(WorldParameters,Amount).
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(WorldParameters::any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WorldParameters) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, WorldParameters).

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
  Args = [ WorldParameters ],
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  SimulationController = {
    simulation_controller,
    {simulation_controller,start_link,Args},
    Restart,
    Shutdown,
    Type,
    [simulation_controller]
  },
  EventStream = {
    simulation_event_stream,
    {simulation_event_stream,start_link,[]},
    Restart,
    Shutdown,
    Type,
    [simulation_event_stream]
  },
  SimulationsSupervisor = {
    simulations_supervisor,
    {simulations_supervisor, start_link,Args},
    Restart,
    brutal_kill,
    supervisor,
    [simulations_supervisor]
  },

  {ok, {SupFlags, [EventStream, SimulationController, SimulationsSupervisor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================