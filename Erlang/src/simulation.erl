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

start(_StartType, _StartArgs) ->
  Parameters = read_world_parameters_from_settings(),
  simulation_main_supervisor:start_link(Parameters).

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
