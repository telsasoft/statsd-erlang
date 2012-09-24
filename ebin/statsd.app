{application, statsd, [
  {description, "An Erlang client for statsd"},
  {vsn, "0.0.1"},
  {modules,
    [ statsd,
      statsd_app,
      statsd_tests
	  ]
	},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {statsd_app, []}},
  {env, []}
]}.