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

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | {error, {already_started, pid()}}).
start_link() ->
  gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).
add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(InitArgs :: term()) ->
  {ok, State :: #state{}} |
  {ok, State :: #state{}, hibernate} |
  {error, Reason :: term()}).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
  {ok, NewState :: #state{}} |
  {ok, NewState :: #state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).

handle_event({pedestrian,Pid,spawned,PedestrianState}, State) ->
  %io:format("Pedestrian ~w spawned at <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  X = 1,
  Y = 1,
  gen_tcp:send(Socket,<<"{\"action\":\"pedestrian_spawned\",\"pid\":",Pid/binary,",\"position_x\":",X,",\"position_y\":",Y,"}">>),
  PedestrianState#pedestrian.position#position.x,
  {ok, State};
handle_event({pedestrian,Pid,disappeared,_PedestrianState}, State) ->
  %io:format("Pedestrian ~w disappeared ~n",[Pid]),
  gen_tcp:send(Socket,<<"{\"action\":\"pedestrian_disappeared\",\"pid\":",Pid/binary,"}">>),
  {ok, State};
handle_event({pedestrian,Pid,move,PedestrianState}, State) ->
  %io:format("Pedestrian ~w moves to <~w,~w> ~n",[Pid,PedestrianState#pedestrian.position#position.x,PedestrianState#pedestrian.position#position.y]),
  gen_tcp:send(Socket,<<"{\"action\":\"pedestrian_move\",\"pid\":",Pid/binary,",\"position_x\":",X,",\"position_y\":",Y,"}">>),
  {ok, State};

handle_event({car,spawned,_CarState}, State) ->
  %io:format("Car spawned ~n"),
  gen_tcp:send(Socket,<<"{\"action\":\"car_spawned\",\"pid\":",Pid/binary,",\"position_x\":",X,",\"position_y\":",Y,"}">>),
  {ok, State};
handle_event({car,disappeared,_CarState}, State) ->
  %io:format("Car disappeard ~n"),
  gen_tcp:send(Socket,<<"{\"action\":\"car_disappeared\",\"pid\":",Pid/binary,"}">>),
  {ok, State};

handle_event({lights,started,_CarState}, State) ->
  %io:format("Lights started ~n"),
  {ok, State};

handle_event({lights,stopped,_CarState}, State) ->
  %io:format("Lights disappeared ~n"),
  {ok, State};

handle_event({lights,changes_to_red,_Data}, State) ->
  %io:format("Main lights are red. Sub lights are green ~n"),
  gen_tcp:send(Socket,<<"{\"action\":\"lights_changes_to_green\"}">>),
  {ok, State};

handle_event({lights,changes_to_yellow,_Data}, State) ->
  %io:format("Main lights are yellow. Sub lights are yellow ~n"),
  {ok, State};

handle_event({lights,changes_to_green,_Data}, State) ->
  %io:format("Main lights are green. Sub lights are red ~n"),
  gen_tcp:send(Socket,<<"{\"action\":\"lights_changes_to_red\"}">>),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
  {ok, Reply :: term(), NewState :: #state{}} |
  {ok, Reply :: term(), NewState :: #state{}, hibernate} |
  {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  {remove_handler, Reply :: term()}).
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), State :: #state{}) ->
  {ok, NewState :: #state{}} |
  {ok, NewState :: #state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).
handle_info(_Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
remove_handler | {error, {'EXIT', Reason :: term()}} |
{error, term()}), State :: term()) -> term()).
terminate(_Arg, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
