%%%-------------------------------------------------------------------
%%% @author Krzysiek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sty 2018 23:28
%%%-------------------------------------------------------------------
-module(socket_event_handler).
-author("Krzysiek").

-behaviour(gen_event).

-include("../include/records.hrl").
%% API
-export([start_link/0,
  add_handler/0]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
  gen_event:start_link({local, ?SERVER}).


add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([Socket|_]) ->
  {ok, #state{socket = Socket}}.

handle_event({pedestrian,Pid,spawned,PedestrianState}, State) ->
  %io:format("Pedestrian ~w spawned at <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  X = integer_to_binary(PedestrianState#pedestrian.position#position.x),
  Y = integer_to_binary(PedestrianState#pedestrian.position#position.y),
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"pedestrian_spawned\",\"pid\":\"",Pid_string/binary,"\",\"position_x\":\"",X/binary,"\",\"position_y\":\"",Y/binary,"\"}">>),
  {ok, State};
handle_event({pedestrian,Pid,disappeared,_PedestrianState}, State) ->
  %io:format("Pedestrian ~w disappeared ~n",[Pid]),
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"pedestrian_disappeared\",\"pid\":\"",Pid_string/binary,"\"}">>),
  {ok, State};
handle_event({pedestrian,Pid,move,PedestrianState}, State) ->
  %io:format("Pedestrian ~w moves to <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  X = integer_to_binary(PedestrianState#pedestrian.position#position.x),
  Y = integer_to_binary(PedestrianState#pedestrian.position#position.y),
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  Speed = integer_to_binary(PedestrianState#pedestrian.world_parameters#world_parameters.pedestrian_speed),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"pedestrian_move\",\"pid\":\"",Pid_string/binary,"\",\"position_x\":\"",X/binary,"\",\"position_y\":\"",Y/binary,"\",\"speed\":\"",Speed/binary,"\"}">>),
  {ok, State};

handle_event({car,Pid,spawned,CarState}, State) ->
  %io:format("Car spawned ~n"),
  X = integer_to_binary(CarState#car.position#position.x),
  Y = integer_to_binary(CarState#car.position#position.y),
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  Turn = erlang:atom_to_binary(CarState#car.destination,utf8),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"car_spawned\",\"pid\":\"",Pid_string/binary,"\",\"position_x\":\"",X/binary,"\",\"position_y\":\"",Y/binary,"\",\"turn\":\"",Turn/binary,"\"}">>),
  {ok, State};
handle_event({car,Pid,disappeared,_CarState}, State) ->
  %io:format("Car disappeard ~n"),
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"car_disappeared\",\"pid\":\"",Pid_string/binary,"\"}">>),
  {ok, State};
handle_event({car,Pid,move,CarState}, State) ->
  %io:format("Pedestrian ~w moves to <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  X = integer_to_binary(CarState#car.position#position.x),
  Y = integer_to_binary(CarState#car.position#position.y),
  Speed = integer_to_binary(CarState#car.world_parameters#world_parameters.car_speed), %
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  Turn = erlang:atom_to_binary(CarState#car.destination,utf8),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"car_move\",\"pid\":\"",Pid_string/binary,"\",\"position_x\":\"",X/binary,"\",\"position_y\":\"",Y/binary,"\",\"turn\":\"",Turn/binary,"\",\"speed\":\"",Speed/binary,"\"}">>),
  {ok, State};
handle_event({car,Pid,turn_left,_CarState}, State) ->
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"car_turn_left\",\"pid\":\"",Pid_string/binary,"\"}">>),
  {ok, State};
handle_event({car,Pid,turn_right,_CarState}, State) ->
  Pid_string = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"car_turn_right\",\"pid\":\"",Pid_string/binary,"\"}">>),
  {ok, State};

handle_event({lights,started,_CarState}, State) ->
  %io:format("Lights started ~n"),
  {ok, State};

handle_event({lights,stopped,_CarState}, State) ->
  %io:format("Lights disappeared ~n"),
  {ok, State};

handle_event({lights,changes_to_red,_Data}, State) ->
  %io:format("Main lights are red. Sub lights are green ~n"),
  %io:format("PID = ~s",State#state.socket),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"lights_changes_to_red\"}">>),
  {ok, State};

handle_event({lights,changes_to_yellow,_Data}, State) ->
  %io:format("Main lights are yellow. Sub lights are yellow ~n"),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"lights_changes_to_yellow\"}">>),
  {ok, State};

handle_event({lights,changes_to_green,_Data}, State) ->
  %io:format("PID = ~s",State#state.socket),
  gen_tcp:send(State#state.socket,<<"{\"action\":\"lights_changes_to_green\"}">>),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.


handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.


handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
