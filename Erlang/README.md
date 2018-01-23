# Crossroad-Simulation
## Description
Simple simulation of crossroad with traffic lights in erlang.

## How to run it ??
1. `rebar3 compile`
2. `erl -pa _build/default/lib/crossroad_simulation/ebin`
3. `application:start(sasl).`
4. `application:start(crossroad_simulation).`
5. `simulation_controller:start_simulation().`
