%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:10
%%%-------------------------------------------------------------------
-module(common_defs).
-author("motek").

%% API
-export([stop_children/1]).

stop_children(SupervisorName) ->
  [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].