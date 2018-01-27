%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jan 2018 13:55
%%%-------------------------------------------------------------------
-module(simulation_event_stream).
-author("motek").

%% API
-export([start_link/0,component_ready/1,notify/3,notify/4,attach_handler/1,remove_handler/1]).

%start_link() ->
%  {ok, Pid} = gen_event:start_link({local, ?MODULE}),

%  gen_event:add_handler(?MODULE, default_event_handler, []),
%  component_ready(?MODULE),
%  {ok,Pid}.

start_link() ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  {ok,Socket} = gen_tcp:listen(12345, [{active,true}, binary,{packet,2}]),
  {ok,AcceptSocket} = gen_tcp:accept(Socket),
  gen_event:add_handler(?MODULE, socket_event_handler, [AcceptSocket]),
  component_ready(?MODULE),
  {ok, Pid}.

component_ready(Name) ->
  gen_event:notify(?MODULE, {Name, ready}).

notify(Name, Action, State) ->
  gen_event:notify(?MODULE, {Name, Action, State}).

notify(Name, Pid, Action, State) ->
  gen_event:notify(?MODULE, {Name, Pid, Action, State}).

attach_handler(Handler) ->
  gen_event:add_handler(?MODULE, Handler, []).

remove_handler(Handler) ->
  gen_event:delete_handler(?MODULE, Handler, []).