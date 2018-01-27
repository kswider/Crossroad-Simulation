# Crossroad-Simulation
## Description
Simple simulation of crossroad with traffic lights in erlang.

## How to run it ??
1. `rebar3 compile`
2. `erl -pa _build/default/lib/crossroad_simulation/ebin`
3. `application:start(sasl).`
4. `application:start(crossroad_simulation).`
5. `simulation_controller:start_simulation().`

## Useful commands ##
1. 'simulation_controller:generate_pedestrians(N)' -> generates N random pedestrians.
2. 'simulation_controller:generate_cars(N)' -> generates N random cars.
3. 'simulation_controller:start_socket_handler' -> starts event handler which writes to the socket.
