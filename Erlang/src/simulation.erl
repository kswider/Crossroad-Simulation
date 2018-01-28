%%%-------------------------------------------------------------------
%%% @author Krzysiek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. sty 2018 14:47
%%%-------------------------------------------------------------------
-module(simulation).
-author("Krzysiek").

-behaviour(application).
-include("../include/records.hrl").

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================


read_world_parameters_from_settings() ->
  MainLightsTime = application:get_env(crossroad_simulation, main_light_time, 10000),
  SubLightsTime = application:get_env(crossroad_simulation, sub_light_time, 10000),
  YellowLightsTime = application:get_env(crossroad_simulation, yellow_light_time, 8000),
  CarsStartAmmount = application:get_env(crossroad_simulation, cars_start_amount,3),
  PedestianStartAmmount = application:get_env(crossroad_simulation, pedestrian_start_amount,1),
  PedestrianSpeed = application:get_env(crossroad_simulation, pedestrian_speed, 2000),
  CarSpeed = application:get_env(crossroad_simulation, car_speed, 1000),

  #world_parameters{
    main_light_time = MainLightsTime,
    sub_light_time = SubLightsTime,
    yellow_light_time = YellowLightsTime,
    cars_start_amount = CarsStartAmmount,
    pedestrian_start_amount = PedestianStartAmmount,
    pedestrian_speed = PedestrianSpeed,
    car_speed = CarSpeed
  }.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  Parameters = read_world_parameters_from_settings(),
  simulation_main_supervisor:start_link(Parameters).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
