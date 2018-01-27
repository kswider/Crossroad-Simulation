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
-export([start_link/1, handle_call/3]).

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

init({WorldParameters,{Position,Directions}}) ->
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

handle_call({are_you_at,PositionList}, _From, State) ->
  {reply, am_i_at(PositionList,State), State};

handle_call({are_you_at,X,Y}, _From, State) ->
  {reply, am_i_at([{X,Y}],State), State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

next_position(State) ->
    NState = estimate_next_position(State),
  case am_i_at_change_position(NState) of
    true ->
      NNState = make_turn(NState),
      NNState;
    _ ->
      NState
  end.

am_i_at_change_position(State) ->
  am_i_at_change_position(State#pedestrian.position,common_defs:get_turn_points(pedestrian,State#pedestrian.world_parameters)).
am_i_at_change_position(_Pos,[]) -> false;
am_i_at_change_position({_,X,Y,_,_},[{_,X,Y,_,_}|_T]) -> true;
am_i_at_change_position(Pos,[_|T]) -> am_i_at_change_position(Pos,T).

make_turn(State) ->
  [H|_T] = State#pedestrian.directions,
  case H of
    forward -> State;
    left ->
      Position = State#pedestrian.position,
      Old_x = Position#position.look_x,
      Old_y = Position#position.look_y,
      State#pedestrian{position =
        Position#position{
          look_x = Old_y,
          look_y = -Old_x
        }
      };
    right ->
      Position = State#pedestrian.position,
      Old_x = Position#position.look_x,
      Old_y = Position#position.look_y,
      State#pedestrian{position =
        Position#position{
          look_x = -Old_y,
          look_y = Old_x
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

am_i_entering_zebra(State) -> am_i_entering_zebra(State#pedestrian.position,
  common_defs:get_waiting_points(pedestrian,State#pedestrian.world_parameters)).
am_i_entering_zebra(_Pos,[]) -> false;
am_i_entering_zebra(Pos,[Pos|_T]) -> true;
am_i_entering_zebra(Pos,[_|T]) -> am_i_entering_zebra(Pos,T).

is_light_green(State) ->
  case gen_statem:call(light_entity,which_lights(State)) of
    green -> true;
    _ -> false
  end.

which_lights(State) ->
  Position = State#pedestrian.position,
  case (Position#position.look_x == 0) of
    true -> get_main_road_lights;
    _ -> get_sub_road_lights
  end.

is_free(State) ->
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  NxtPosition = (next_position(State))#pedestrian.position,
  case common_defs:ask_cars_for_position(Cars,NxtPosition#position.x,NxtPosition#position.y,self()) of
    free -> true;
    _ -> false
  end.

am_i_at([],_State) -> false;
am_i_at([{X,Y}|_PositionTail],State) when
  (State#pedestrian.position#position.x == X) and (State#pedestrian.position#position.y == Y) ->
  true;
am_i_at([_|PositionTail],State) -> am_i_at(PositionTail,State).