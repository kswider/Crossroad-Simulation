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
-export([start_link/1,generate_pedestrians/2,kill_children/0,spawn_pedestrian/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
generate_pedestrians(_WorldParameters,0) ->
  done;
generate_pedestrians(WorldParameters,Amount) ->
  spawn(?MODULE,spawn_pedestrian,[WorldParameters, gen_server:call(uuid_provider,next_pedestrian), common_defs:get_random(pedestrian,WorldParameters)]),
  generate_pedestrians(WorldParameters,Amount-1).

kill_children() -> common_defs:stop_children(?MODULE).

start_link(WorldParameters) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, WorldParameters).

spawn_pedestrian(WorldParameters,UUID,PedestrianParams) ->
  Pedestrians = supervisor:which_children(simulation_pedestrians_supervisor),
  {Pos,_Dest} = PedestrianParams,
  case common_defs:ask_pedestrians_for_position(Pedestrians,Pos#position.x,Pos#position.y) of
    free ->
      Pedestrian = { {pedestrian, UUID},
        {pedestrian_entity, start_link, [{WorldParameters,PedestrianParams}]},
        temporary, brutal_kill, worker,
        [ pedestrian_entity ]},
      supervisor:start_child(?MODULE, Pedestrian);
    _ ->
      timer:sleep(1000),
      spawn_pedestrian(WorldParameters,UUID,PedestrianParams)
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
