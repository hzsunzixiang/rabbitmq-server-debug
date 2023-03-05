-module(thoas_test).
-compile([export_all]).
-compile(nowarn_export_all).




start() ->
   E1 = thoas:encode(#{age => 44, name => <<"Steve Irwin">>, nationality => <<"Australian">>}),
   io:format("E1:~p~n", [E1]),
   D1 = thoas:decode(<<"{\"age\":44,\"name\":\"Steve Irwin\",\"nationality\":\"Australian\"}">>),
   io:format("D1:~p~n", [D1]),
   finish.
