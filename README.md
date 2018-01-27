# Crossroad-Simulation
## Description
Simple simulation of crossroad with traffic lights reacting to traffic on the road written in erlang.

## How to run it ??

Invoke following commands in terminal:

1. `cd Erlang`
2. `rebar3 compile`
3. `erl -pa _build/default/lib/crossroad_simulation/ebin`

Invoke following commands in the Erlang shell:

4. `application:start(sasl).`
5. `application:start(crossroad_simulation).`
6. `simulation_controller:start_simulation().`
 
## Graphical interface ##

If you want to start graphical handler for the simulation you need to:

1. Invoke `simulation_controller:start_socket_handler().` in the Erlang shell.
2. Start Unity build or other compatible with this simulation.

Be careful! If you will change order of 1 and 2 handler won't work!

## Changing world parameters##

You can change this parameters in simulation by invoking 'application:set_env(crossroad_simulation,Param,Val):
-`main_light_time` - default time of green light on main road in milisecons
-`sub_light_time` - default time of green light on sub road in milisecons
-`yellow_light_time` - default time of yellow light in milisecons
-`cars_start_amount` - amount of cars spawned while stating the simulation
-`pedestrian_start_amount` - amount of pedestrians spawned while stating the simulation
-`pedestrian_speed` - pedestrians refresh duration in milisecons
-`car_speed` - cars refresh duration in milisecons


## Useful commands ##

Commands can be invoked in Erlang shell when simulation is started. 
1. `simulation_controller:generate_pedestrians(N)` -> generates N random pedestrians.
2. `simulation_controller:generate_cars(N)` -> generates N random cars.

## Project architecture ##

- `simulation` - rebar main process
 - `simulation_main_supervisor` - Root of supervision hierarchy.
   - `simulation_controller` - Server which is responsible for handling simulation state and sending commands to the simulation subsystems from the outside.
   - `simulation_event_stream` - Events notifier, which is responsible for notifying about all state changes during the simulation.
     - `default_event_handler` - gen_event server responsible for printing all events into console. Always stated with the application
     - `socket_event_handler` - gen_event server responsible for wrining all events into pre-defined socket. Started by appropriate command.
   - `simulations_supervisor` - Supervisor for all necessary simulation entities.
     - `pedestrians_supervisor` - Supervisor responsible for all pedestrians on the map.
       - `pedestrian_entity` - gen_server server responsible for single pedestrian on the map.
     - `traffic_supervisor` - Supervisor responsible for all cars on the map.
       - `car_entity` - gen_server server responsible for single car on the map.
     - `light_entity` - gen_statem server responsible for changing lights periodicly and adapting them to the traffic on the road.
     - `UUIDProvider` - gen_server server responsible providing unique ID for new processes.

##How it works##

When simulation starts lights are being started automathicly. Lights are one big state machine with 3 states (red, yellow, green) which tries to change its state every N seconds (N can be different for each state).
If there are some objects on both roads ligth color always changes. If crossing is empty main road lights are always green. If there is someone on the sub road and main road is free sub, lights will be green until sub road is empty or someone appears on the main road. 

Cars are spawned on the map in pre-defined points. Every N-seconds (N is car_speed parameter in world_parameter) car is trying to make a step. In order to do this, car:
1. Asks other cars if they are not on the pool where they want to go
2. If car enters crossing it checks light color and if there are no pedestrians left on the zebra.
3. If car is on the pool where it should make a turn (those points are also pre-defined) it makes a turn or tries to synchronise itself with other cars on the crossing (which is importanat especially while turning left)

Pedestrians are very similar to cars but there can be more then one pedestrian on single map pool. Therefore pedestrians are not aware of each other but they check if they can safely enter crossroad.

##Project problems##

Due to very high load of messeges being send between cars, lights and pedestians simulation can easily broken (when cars gets timeout response it waits because it can't say if there is someone on the next pool). This could be solved by splitting `simulation_pedestrians_supervisor` and `simulation_traffic_supervisor` into more supervisors (e.g. simulation_north_road_pedestrians_superisor, simulation_south_road_pedestrians_supervisor, simulation_crossing_pedestrians_supervisor, simulation_waiting_to_spawn_pedestians_superisor etc.).
