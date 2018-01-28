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
-export([start_link/1,make_step/1]).

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

init({WorldParameters,{Position,Destination}}) ->
  Pid = self(),
  TimerRef = erlang:start_timer(WorldParameters#world_parameters.car_speed, Pid, make_next_step),
  State = #car{
    pid = Pid,
    position = Position,
    destination = Destination,
    world_parameters = WorldParameters,
    timer_ref = TimerRef,
    making_move = false
  },
  simulation_event_stream:notify(car,Pid,spawned,State),
  {ok, State}.

handle_call({are_you_at,PositionList}, _From, State) ->
  {reply, am_i_at(PositionList,State), State};

handle_call({are_you_at,X,Y}, _From, State) ->
  {reply, am_i_at([{X,Y}],State), State};

handle_call({will_you_be_at,X,Y}, _From, State) ->
  {reply, am_i_at([{X,Y}],estimate_next_position(State)), State};

handle_call({do_you_turn_left}, _From, State) ->
  if
    State#car.destination == left ->
      erlang:cancel_timer(State#car.timer_ref),
      NState = next_position(State),
      TimerRef = erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, State#car.pid, make_next_step),
      {reply, true, NState#car{timer_ref = TimerRef}};
    true ->
      {reply, false, State}
  end;

handle_call(Request, From, State) ->
  simulation_event_stream:notify(car,State#car.pid,{unknown_call,Request,From},State),
  {reply, false, State}.

handle_cast(move,State) ->
  NState = next_position(State),
  TimerRef = erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, State#car.pid, make_next_step),
  {noreply, NState#car{timer_ref = TimerRef}};
handle_cast(wait,State) ->
  simulation_event_stream:notify(car,State#car.pid,waits,State),
  TimerRef = erlang:start_timer(State#car.world_parameters#world_parameters.car_speed, State#car.pid, make_next_step),
  {noreply, State#car{timer_ref = TimerRef}};
handle_cast(timeout,State) ->
  simulation_event_stream:notify(car,State#car.pid,got_timeout,State),
  TimerRef = erlang:start_timer(round(rand:uniform()*100), State#car.pid, make_next_step),
  {noreply, State#car{timer_ref = TimerRef}};
handle_cast(Request, State) ->
  simulation_event_stream:notify(car,State#car.pid,{unknown_cast,Request},State),
  {noreply, State}.

handle_info({timeout, _Ref, make_next_step}, State) ->
  case common_defs:should_dissapear(State#car.world_parameters,State#car.position) of
    true -> supervisor:terminate_child(simulation_traffic_supervisor,State#car.pid);
    _ ->
      spawn(?MODULE,make_step,[State]),
      {noreply,State}
  end;
handle_info(Request,State) ->
  simulation_event_stream:notify(car,State#car.pid,{unknown_call,Request},State),
  {noreply,State}.

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
  ((State#car.position#position.x == X) and (State#car.position#position.y == Y)) or
    ((State#car.position#position.x - State#car.position#position.look_x == X) and (State#car.position#position.y - State#car.position#position.look_y == Y))->
  true;
am_i_at([_|PositionTail],State) -> am_i_at(PositionTail,State).

next_position(State) ->
  NState = estimate_next_position(State),
  simulation_event_stream:notify(car,State#car.pid,move,NState),
  case am_i_at_change_position(NState) of
    true ->
      case NState#car.destination of
        left -> simulation_event_stream:notify(car,State#car.pid,turn_left,NState);
        right -> simulation_event_stream:notify(car,State#car.pid,turn_right,NState);
        _ -> simulation_event_stream:notify(car,State#car.pid,turn_forward,NState)
      end,
      NNState = make_turn(NState),
      NNState;
    _ ->
      NState
  end.

am_i_at_change_position(State) ->
  am_i_at_change_position(State#car.position,common_defs:get_turn_points(car,State#car.world_parameters)).
am_i_at_change_position(_Pos,[]) -> false;
am_i_at_change_position({_,X,Y,_,_},[{_,X,Y,_,_}|_T]) -> true;
am_i_at_change_position(Pos,[_|T]) -> am_i_at_change_position(Pos,T).

make_turn(State) ->
  case State#car.destination of
    forward -> State;
    left ->
      Position = State#car.position,
      Old_x = Position#position.look_x,
      Old_y = Position#position.look_y,
      NState = State#car{position =
      Position#position{
        look_x = my_round((math:sqrt(2) / 2) * Old_x - (math:sqrt(2) / 2) * Old_y),
        look_y = my_round((math:sqrt(2) / 2) * Old_x + (math:sqrt(2) / 2) * Old_y)
      }
      },
      NState;
    right ->
      Position = State#car.position,
      Old_x = Position#position.look_x,
      Old_y = Position#position.look_y,
      NState = State#car{position =
      Position#position{
        look_x = Old_y,
        look_y = -Old_x
      }
      },
      NState;
    _ -> State
  end.

estimate_next_position(State) ->
  Position = State#car.position,
  State#car{position = Position#position{
    x = Position#position.x + Position#position.look_x,
    y = Position#position.y + Position#position.look_y
  }}.

am_i_entering_crossing(State) -> am_i_entering_crossing(State#car.position,common_defs:get_waiting_points(car,State#car.world_parameters)).
am_i_entering_crossing(_Pos,[]) -> false;
am_i_entering_crossing(Pos,[Pos|_T]) -> true;
am_i_entering_crossing(Pos,[_|T]) -> am_i_entering_crossing(Pos,T).

is_light_green(State) ->
  try gen_statem:call(light_entity,which_lights(State)) of
    green -> true;
    _ -> false
  catch
    exit:_Reason ->
      false
  end.

which_lights(State) ->
  Position = State#car.position,
  case (Position#position.look_y /= 0) of
    true -> get_main_road_lights;
    _ -> get_sub_road_lights
  end.

is_free(State) ->
  Cars = supervisor:which_children(simulation_traffic_supervisor),
  Pedestrians = supervisor:which_children(simulation_pedestrians_supervisor),
  NxtState = estimate_next_position(State),
  NxtPosition = NxtState#car.position,
  CarsResponse = common_defs:ask_cars_for_position(Cars,NxtPosition#position.x,NxtPosition#position.y,State#car.pid),
  PedestriansResponse = common_defs:ask_pedestrians_for_position(Pedestrians,NxtPosition#position.x,NxtPosition#position.y),
  case CarsResponse of
    timeout ->
      timeout;
    free ->
      if
        ((State#car.destination == left) and (State#car.position#position.look_x /= 0) and (State#car.position#position.look_y /= 0)) ->
          CarsResponse3 = common_defs:ask_cars_for_position_2(Cars,NxtPosition#position.x,NxtPosition#position.y,State#car.pid),
          simulation_event_stream:notify(asked,future,CarsResponse3),
          case CarsResponse3 of
            free -> true;
            timeout -> timeout;
            _ -> false
          end;
        true ->
          (PedestriansResponse == free)
      end;
    CarPid ->
      if
        ((State#car.destination == left) and (State#car.position#position.look_x /= 0) and (State#car.position#position.look_y /= 0)) ->
          try gen_server:call(CarPid,{do_you_turn_left},300) of
            true ->
              true;
            _ ->
              false
          catch
            exit:_Reason ->
              timeout
          end;
        true ->
          false
      end
  end.

my_round(X) when X < 0 -> -1;
my_round(X) when X == 0 -> 0;
my_round(X) when X > 0 -> 1.


make_step(State) ->
  case is_free(State) of
    true ->
      case am_i_entering_crossing(State) of
        true ->
          case is_light_green(State) of
            true ->
              gen_server:cast(State#car.pid,move);
            _ ->
              gen_server:cast(State#car.pid,wait)
          end;
        _ ->
          gen_server:cast(State#car.pid,move)
      end;
    timeout ->
      gen_server:cast(State#car.pid,timeout);
    _ ->
      gen_server:cast(State#car.pid,wait)
  end.