%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:50
%%%-------------------------------------------------------------------
-author("motek").

-define(PEDESTRIAN_START_POSITIONS,[{5,0,0,1},{8,0,0,1},{0,5,1,0},{13,5,-1,0},{0,8,1,0},{13,8,-1,0},{5,13,0,-1},{8,13,0,-1}]).

-record(position,{x,y,look_x,look_y}).
-record(world_parameters,{main_light_time,sub_light_time,yellow_light_time,cars_start_amount,pedestrian_start_amount,pedestrian_speed}).
-record(light,{pid,main_road,waiting = [],world_parameters}).
-record(pedestrian,{pid,directions = [], position = #position{} ,world_parameters}).