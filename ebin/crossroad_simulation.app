{ application,crossroad_simulation,
  [ { description, "Some simple simulation" },
    { vsn, "1.0" },
    { modules, [car_entity, common_defs, default_event_handler, light_entity, pedestrian_entity, simulation,
      simulation_controller, simulation_event_stream, simulation_lights_supervisor, simulation_main_supervisor,
      simulation_pedestrians_supervisor, simulation_traffic_supervisor, simulations_supervisor] },
    { registered, [] },
    { applications, [ kernel, stdlib, sasl ] },
    { env, [] },
    { mod, { simulation, [] } }
  ]
}.