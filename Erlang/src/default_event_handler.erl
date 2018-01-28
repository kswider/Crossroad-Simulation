%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jan 2018 13:56
%%%-------------------------------------------------------------------
-module(default_event_handler).
-author("motek").

-behaviour(gen_event).

-include("../include/records.hrl").

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, {}}.

handle_event({pedestrian,Pid,spawned,PedestrianState}, State) ->
  io:format("Pedestrian ~w spawned at <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  {ok, State};
handle_event({pedestrian,Pid,disappeared,_PedestrianState}, State) ->
  io:format("Pedestrian ~w disappeared ~n",[Pid]),
  {ok, State};
handle_event({pedestrian,Pid,move,PedestrianState}, State) ->
  io:format("Pedestrian ~w moves to <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  {ok, State};
handle_event({pedestrian,Pid,waits,PedestrianState}, State) ->
  io:format("Pedestrian ~w waits at <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  {ok, State};

handle_event({car,Pid,spawned,_CarState}, State) ->
  io:format("Car ~w spawned ~n",[Pid]),
  {ok, State};
handle_event({car,Pid,disappeared,_CarState}, State) ->
  io:format("Car ~w disappeard ~n",[Pid]),
  {ok, State};
handle_event({car,Pid,move,CarState}, State) ->
  io:format("Car ~w moves to <~w,~w> ~n",[Pid,CarState#car.position#position.x,CarState#car.position#position.y]),
  {ok, State};
handle_event({car,Pid,waits,CarState}, State) ->
  io:format("Car ~w waits at <~w,~w> ~n",[Pid,CarState#car.position#position.x,CarState#car.position#position.y]),
  {ok, State};
handle_event({car,Pid,turn_left,CarState}, State) ->
  io:format("Car ~w turns ~w ~n",[Pid,CarState#car.destination]),
  {ok, State};
handle_event({car,Pid,turn_right,CarState}, State) ->
  io:format("Car ~w turns ~w ~n",[Pid,CarState#car.destination]),
  {ok, State};

handle_event({lights,started,_CarState}, State) ->
  io:format("Lights started ~n"),
  {ok, State};
handle_event({lights,stopped,_CarState}, State) ->
  io:format("Lights disappeared ~n"),
  {ok, State};
handle_event({lights,changes_to_red,_Data}, State) ->
  io:format("Main lights are red. Sub lights are green ~n"),
  {ok, State};
handle_event({lights,changes_to_yellow,_Data}, State) ->
  io:format("Main lights are yellow. Sub lights are yellow ~n"),
  {ok, State};
handle_event({lights,changes_to_green,_Data}, State) ->
  io:format("Main lights are green. Sub lights are red ~n"),
  {ok, State};
handle_event({lights,remaining_green,_Data}, State) ->
  io:format("Main lights are green. Sub lights are red ~n"),
  {ok, State};
handle_event({lights,remaining_red,_Data}, State) ->
  io:format("Main lights are red. Sub lights are green ~n"),
  {ok, State};

handle_event(Msg,State) ->
  io:format("Not handled event happned!  <~w> ~n",[Msg]),
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
