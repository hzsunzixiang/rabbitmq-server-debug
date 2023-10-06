{application, 'erl_signal_test', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['erl_signal_test_app','erl_signal_test_sup','rabbit_prelaunch_sighandler']},
	{registered, [erl_signal_test_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {erl_signal_test_app, []}},
	{env, []}
]}.