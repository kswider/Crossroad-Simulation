%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:07
%%%-------------------------------------------------------------------
-module(car_entity).
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

%%%===================================================================
%%% API
%%%===================================================================
start_link(InitialState) ->
  gen_server:start_link(?MODULE, InitialState, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({WorldParameters,Position,Destination}) ->
  State = #car{
    pid = self(),
    position = Position,
    destination = Destination,
    world_parameters = WorldParameters
  },
  simulation_event_stream:notify(car,self(),spawned,State),
  erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, self(), make_next_step),
  {ok, State}.

handle_call({are_you_at,PositionList}, _From, State) ->
  {reply, am_i_at(PositionList,State), State};

handle_call({are_you_at,X,Y}, _From, State) ->
  {reply, am_i_at([{X,Y}],State), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, make_next_step}, State) ->
  case common_defs:should_dissapear(State#car.world_parameters,State#car.position) of
    true -> supervisor:terminate_child(simulation_traffic_supervisor,State#car.pid);
    _ ->
      case am_i_entering_crossing(State) of
        true ->
          case is_light_green(State) and is_free(State) of
            true ->
              NState = next_position(State),
              simulation_event_stream:notify(car,State#car.pid,move,State),
              erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, self(), make_next_step),
              {noreply, NState};
            _ ->
              simulation_event_stream:notify(car,State#car.pid,waits,State),
              erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, self(), make_next_step),
              {noreply, State}
          end;
        _ ->
          NState = next_position(State),
          simulation_event_stream:notify(car,State#car.pid,move,State),
          erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, self(), make_next_step),
          {noreply, NState}
      end
  end.

terminate(_Reason, State) ->
  simulation_event_stream:notify(car,State#car.pid,disappeared,State),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
am_i_at([],_State) -> false;
am_i_at([{X,Y}|_PositionTail],State) when
  (State#car.position#position.x == X) and (State#car.position#position.y == Y) ->
  true;
am_i_at([_|PositionTail],State) -> am_i_at(PositionTail,State).

next_position(State) ->
  NState = estimate_next_position(State),
  case am_i_at_change_position(State) of
    true ->
      NNState = make_turn(NState),
      NNState;
    _ ->
      NState
  end.

am_i_at_change_position(State) ->
  {X,Y} = {State#car.position#position.x,State#car.position#position.y},
  am_i_at_change_position({X,Y},common_defs:get_cars_turn_points(State#car.world_parameters)).
am_i_at_change_position(_Pos,[]) -> false;
am_i_at_change_position(Pos,[Pos|_T]) -> true;
am_i_at_change_position(Pos,[_|T]) -> am_i_at_change_position(Pos,T).

make_turn(State) ->
  case State#car.destination of
    forward -> State;
    left ->
      Position = State#car.position,
      State#car{position =
      Position#position{
        look_x = Position#position.look_y,
        look_y = -Position#position.look_x
      }
      };
    right ->
      Position = State#car.position,
      State#car{position =
      Position#position{
        look_x = -Position#position.look_y,
        look_y = Position#position.look_x
      }
      };
    _ -> State
  end.

estimate_next_position(State) ->
  Position = State#car.position,
  State#car{position = Position#position{
    x = Position#position.x + Position#position.look_x,
    y = Position#position.y + Position#position.look_y
  }}.

am_i_entering_crossing(State) -> am_i_entering_crossing({State#car.position#position.x,State#car.position#position.y},common_defs:get_cars_enter_crossing_points(State#car.world_parameters)).
am_i_entering_crossing(_Pos,[]) -> false;
am_i_entering_crossing(Pos,[Pos|_T]) -> true;
am_i_entering_crossing(Pos,[_|T]) -> am_i_entering_crossing(Pos,T).

is_light_green(State) ->
  Position = State#car.position,
  case gen_statem:call(light_entity,which_lights(Position)) of
    green -> true;
    _ -> false
  end.

which_lights(State) ->
  Position = State#car.position,
  case ((Position#position.look_x == 1) and (Position#position.look_y == 0)) of
    true -> get_main_road_lights;
    _ -> get_sub_road_lights
  end.

is_free(State) ->
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  Pedestrians = supervisor:which_children(simulation_pedestrians_supervisor),
  NxtPosition = next_position(State),
  (common_defs:ask_cars_for_position(Cars,NxtPosition#position.x,NxtPosition#position.y) == free) and (common_defs:ask_pedestrians_for_position(Pedestrians,NxtPosition#position.x,NxtPosition#position.y) == free).