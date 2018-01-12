%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:07
%%%-------------------------------------------------------------------
-module(pedestrian_entity).
-author("motek").

-behaviour(gen_server).

-include("../include/records.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(WorldParameters::any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(InitialState) ->
  gen_server:start_link(?MODULE, InitialState, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({WorldParameters,Position,Directions}) ->
  State = #pedestrian{
    pid = self(),
    position = Position,
    directions = Directions,
    world_parameters = WorldParameters
  },
  simulation_event_stream:notify(pedestrian,self(),spawned,State),
  erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({are_you_at,X,Y}, From, State) ->
  {reply, am_i_at(X,Y,State), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, _Ref, make_next_step}, State) ->
  case am_i_entering_zebra(State#pedestrian.position) of
    true ->
      case is_light_green(State#pedestrian.position) and is_free(State#pedestrian.position) of
        true ->
          {NDirections,NPosition} = next_position(State#pedestrian.directions, State#pedestrian.position),
          NState = State#pedestrian{directions = NDirections, position = NPosition},
          simulation_event_stream:notify(pedestrian,State#pedestrian.pid,move,State),
          erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
          {noreply, NState};
        _ ->
         simulation_event_stream:notify(pedestrian,self(),waits,State),
         erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
         {noreply, State}
      end;
    _ ->
      {NDirections,NPosition} = next_position(State#pedestrian.directions, State#pedestrian.position),
      NState = State#pedestrian{directions = NDirections, position = NPosition},
      simulation_event_stream:notify(pedestrian,State#pedestrian.pid,move,State),
      erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
      {noreply, NState}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
  simulation_event_stream:notify(pedestrian,disappeared,State),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
am_i_at(X,Y,State) ->
  (State#pedestrian.position#position.x == X) and (State#pedestrian.position#position.y == Y).

next_position([NxtTurn|Tail],Position) ->
    NPosition = estimate_next_position(Position),
  case am_i_at_change_position(Position) of
    true ->
      NNPosition = make_turn(NPosition,NxtTurn),
      {Tail,NNPosition};
    _ ->
      {[NxtTurn|Tail],NPosition}
  end.

am_i_at_change_position(Position) ->
     ((Position#position.x == 5) and (Position#position.y == 5)) or
     ((Position#position.x == 7) and (Position#position.y == 5)) or
     ((Position#position.x == 5) and (Position#position.y == 7)) or
     ((Position#position.x == 7) and (Position#position.y == 7)).

make_turn(Position,forward) -> Position;
make_turn(Position,left) ->
  Position#position{
    look_x = Position#position.look_y,
    look_y = -Position#position.look_x
  };
make_turn(Position,right) ->
  Position#position{
    look_x = -Position#position.look_y,
    look_y = Position#position.look_x
  };
make_turn(Position,_) -> Position.

estimate_next_position(Position) ->
  Position#position{
    x = Position#position.x + Position#position.look_x,
    y = Position#position.y + Position#position.look_y
  }.

am_i_entering_zebra(Position) -> am_i_at_change_position(Position).

is_light_green(Position) ->
  [ {_, Light, _, _} | _ ] = supervisor:which_children(simulation_lights_supervisor),
  case gen_statem:call(Light,which_lights(Position)) of
    green -> true;
    _ -> false
  end.

which_lights(Position) when (Position#position.look_x == 1) and (Position#position.look_y == 0) ->
  get_sub_road_lights;
which_lights(Position) ->
  get_main_road_lights.

is_free(Position) -> true.
  %Cars = supervisor:which_children(simulation_traffic_supervisor),
  %NxtPosition = next_position(Position,[forward]),
  %case ask_cars_for_position(Cars,NxtPosition#position.x,NxtPosition#position.y) of
  %  free -> true;
  %  _ -> false
  %end.

ask_cars_for_position([], NxtPositionPositionX, NxtPositionPositionY) -> free;
ask_cars_for_position([ {_Id, Car, _Type, _Modules} | Rest ], NxtPositionPositionX, NxtPositionPositionY) ->
  try gen_server:call(Car, {are_you_at, NxtPositionPositionX, NxtPositionPositionY}) of
    true ->
      not_free;
    false ->
      ask_cars_for_position(Rest,NxtPositionPositionX,NxtPositionPositionY)
  catch
    exit:_Reason -> not_free
  end.