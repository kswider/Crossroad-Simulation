%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:01
%%%-------------------------------------------------------------------
-module(simulation_controller).
-author("motek").

-behaviour(gen_server).

%% API
-export([start_link/1, start_simulation/0,stop_simulation/0,generate_cars/1,generate_pedestrians/1,start_socket_handler/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_simulation() ->
  gen_server:call(?MODULE, start_simulation).

stop_simulation() ->
  gen_server:call(?MODULE, stop_simulation).

generate_cars(Amount) ->
  gen_server:call(?MODULE, {generate_cars,Amount}).

generate_pedestrians(Amount) ->
  gen_server:call(?MODULE, {generate_pedestrians,Amount}).

start_socket_handler() ->
  gen_server:call(?MODULE, {start_socket_handler}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(WorldParameters::any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WorldParameters) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, WorldParameters, []).

%%%===================================================================
%%% gen_server callbacks
%%%=================================================================
init(WorldParameters) ->
  {ok, {stopped,WorldParameters}}.

handle_call(start_socket_handler, _From, State) ->
  simulation_main_supervisor:add_socket_handler(),
  {reply, started, State};

handle_call(start_simulation, _From, {stopped,WorldParameters}) ->
  simulation_main_supervisor:start_simulation(WorldParameters),
  {reply, started, {started,WorldParameters}};

handle_call(start_simulation, _From, {started,WorldParameters}) ->
  {reply, already_started, {started,WorldParameters}};

handle_call(stop_simulation, _From, {started,WorldParameters}) ->
  simulation_main_supervisor:stop_simulation(),
  {reply, stopped, {stopped,WorldParameters}};

handle_call(stop_simulation, _From, {stopped,WorldParameters}) ->
  {reply, already_stopped, {stopped,WorldParameters}};

handle_call({generate_pedestrians, Amount}, _From, {started,WorldParameters}) ->
  simulation_main_supervisor:generate_pedestrians(WorldParameters,Amount),
  {reply, pedestrians_generated, {started,WorldParameters}};

handle_call({generate_pedestrians, _Amount}, _From, {stopped,WorldParameters}) ->
  {reply, pedestrians_not_generated, {stopped,WorldParameters}};

handle_call({generate_cars, Amount}, _From, {started,WorldParameters}) ->
  simulation_main_supervisor:generate_cars(WorldParameters,Amount),
  {reply, cars_generated, {started,WorldParameters}};

handle_call({generate_cars, _Amount}, _From, {stopped,WorldParameters}) ->
  {reply, cars_not_generated, {stopped,WorldParameters}}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================