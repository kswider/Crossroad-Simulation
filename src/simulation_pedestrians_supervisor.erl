%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:05
%%%-------------------------------------------------------------------
-module(simulation_pedestrians_supervisor).
-author("motek").

-behaviour(supervisor).
-include("../include/records.hrl").

%% API
-export([start_link/1,generate_pedestrians/2,kill_children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
generate_pedestrians(WorldParameters,0) -> done;
generate_pedestrians(WorldParameters,Amount) ->
  Pedestrian = { {pedestrian, Amount},
    {pedestrian_entity, start_link, [ WorldParameters ]},
    temporary, brutal_kill, worker,
    [ pedestrian_entity ]},
  supervisor:start_child(?MODULE, Pedestrian),
  generate_pedestrians(WorldParameters,Amount-1).
kill_children() -> common_defs:stop_children(?MODULE).
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
  simulation_event_stream:component_ready(?MODULE),

  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  %TODO: Check if SupFlags is ok (in rabbits restarts are loaded from world parameters, i dont know why :()
  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
