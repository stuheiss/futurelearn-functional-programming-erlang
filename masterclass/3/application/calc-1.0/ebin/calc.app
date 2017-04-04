{application, calc,
 [{descripton, "Calc application"},
  {vsn, "1.0"},
  {modules, [calc, calc_sup, calc_app, rpn]},
  {registered, [calc_sup, calc]},
  {applications, [kernel, stdlib]},
  {env, [{env, [{a, 23}, {b, -12}]}]},
  {mod, {calc_app, []}}]}.
