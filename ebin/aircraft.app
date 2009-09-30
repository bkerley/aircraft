{application, aircraft,
 [{description, "aIRCraft IRC server"},
  {vsn, "0.0.1"},
  {modules, [aircraft,
             air_server,
             air_sup]},
  {registered, [air_sup]},
  {applications, [kernel, stdlib]},
  {mod, {aircraft, []}}]}.
