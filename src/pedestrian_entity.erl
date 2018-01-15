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

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, make_next_step}, State) ->
  case common_defs:should_dissapear(State#pedestrian.world_parameters,State#pedestrian.position) of
    true -> supervisor:terminate_child(simulation_pedestrians_supervisor,State#pedestrian.pid);
    _ ->
      case am_i_entering_zebra(State) of
        true ->
          case is_light_green(State) and is_free(State) of
            true ->
              NState = next_position(State),
              simulation_event_stream:notify(pedestrian,State#pedestrian.pid,move,State),
              erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
              {noreply, NState};
            _ ->
             simulation_event_stream:notify(pedestrian,State#pedestrian.pid,waits,State),
             erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
             {noreply, State}
          end;
        _ ->
          NState = next_position(State),
          simulation_event_stream:notify(pedestrian,State#pedestrian.pid,move,State),
          erlang:start_timer(State#pedestrian.world_parameters#world_parameters.pedestrian_speed, self(), make_next_step),
          {noreply, NState}
      end
  end.

terminate(_Reason, State) ->
  simulation_event_stream:notify(pedestrian,State#pedestrian.pid,disappeared,State),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
am_i_at(X,Y,State) ->
  (State#pedestrian.position#position.x == X) and (State#pedestrian.position#position.y == Y).

next_position(State) ->
    [NxtTurn|Tail] = State#pedestrian.directions,
    Position = State#pedestrian.position,
    NState = estimate_next_position(State),
  case am_i_at_change_position(State) of
    true ->
      NNState = make_turn(NState),
      NNState;
    _ ->
      NState
  end.

am_i_at_change_position(State) ->
  {X,Y} = {State#pedestrian.position#position.x,State#pedestrian.position#position.y},
  am_i_at_change_position({X,Y},common_defs:get_pedestrian_turn_points(State#pedestrian.world_parameters)).
am_i_at_change_position(Pos,[]) -> false;
am_i_at_change_position(Pos,[Pos|T]) -> true;
am_i_at_change_position(Pos,[_|T]) -> am_i_entering_zebra(Pos,T).

make_turn(State) ->
  [H|T] = State#pedestrian.directions,
  case H of
    forward -> State;
    left ->
      Position = State#pedestrian.position,
      State#pedestrian{position =
        Position#position{
          look_x = Position#position.look_y,
          look_y = -Position#position.look_x
        }
      };
    right ->
      Position = State#pedestrian.position,
      State#pedestrian{position =
        Position#position{
          look_x = -Position#position.look_y,
          look_y = Position#position.look_x
        }
      };
    _ -> State
  end.

estimate_next_position(State) ->
  Position = State#pedestrian.position,
  State#pedestrian{position = Position#position{
    x = Position#position.x + Position#position.look_x,
    y = Position#position.y + Position#position.look_y
  }}.

am_i_entering_zebra(State) -> am_i_at_change_position({State#pedestrian.position#position.x,State#pedestrian.position#position.y},common_defs:get_pedestrian_enter_crossing_points(State#pedestrian.world_parameters)).
am_i_entering_zebra(Pos,[]) -> false;
am_i_entering_zebra(Pos,[Pos|T]) -> true;
am_i_entering_zebra(Pos,[_|T]) -> am_i_entering_zebra(Pos,T).

is_light_green(State) ->
  Position = State#pedestrian.position,
  case gen_statem:call(light_entity,which_lights(Position)) of
    green -> true;
    _ -> false
  end.

which_lights(State) ->
  Position = State#pedestrian.position,
  case ((Position#position.look_x == 1) and (Position#position.look_y == 0)) of
    true -> get_sub_road_lights;
    _ -> get_main_road_lights
  end.

is_free(State) ->
  Position = State#pedestrian.position,
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  NxtPosition = next_position(State),
  case ask_cars_for_position(Cars,NxtPosition#position.x,NxtPosition#position.y) of
    free -> true;
    _ -> false
  end.

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