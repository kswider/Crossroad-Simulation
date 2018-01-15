%%%-------------------------------------------------------------------
%%% @author motek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 15:50
%%%-------------------------------------------------------------------
-author("motek").

-record(position,{x,y,look_x,look_y}).
-record(world_parameters,{world_width, world_height, main_light_time,sub_light_time,yellow_light_time,cars_start_amount,pedestrian_start_amount,pedestrian_speed, car_speed}).
-record(light,{pid,main_road,waiting = [],world_parameters}).
-record(pedestrian,{pid,directions = [], position = #position{} ,world_parameters}).
-record(car,{pid,destination,position = #position{}, world_parameters}).