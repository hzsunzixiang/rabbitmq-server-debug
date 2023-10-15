

模拟RabbitMQ的代码结构: 最外层没有代码

wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap 


# 获取所有的 ebin
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ for i in $(find ~/program/rabbitmq-server-debug/book/erlang_systemd/   -name ebin); do echo -pa $i; done |tr '\n'  ' '


ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/syslog/syslog_erlang (main)$ erl -pa ./.eng.mk/rebar3/_build/bootstrap/lib/cf/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/cth_readable/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/certifi/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/eunit_formatters/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/getopt/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/providers/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/bbmustache/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/ssl_verify_fun/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/relx/ebin -pa ./.erlang.mk/rebar3/_build/bootstrap/lib/erlware_commons/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/providers/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/getopt/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/cf/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/erlware_commons/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/certifi/ebin -pa ./.erlang.mk/rebar3/_build/default/lib/rebar/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/cf/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/cth_readable/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/certifi/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/eunit_formatters/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/getopt/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/providers/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/bbmustache/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/ssl_verify_fun/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/relx/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/erlware_commons/ebin -pa ./.erlang.mk/rebar3/_build/prod/lib/rebar/ebin -pa ./deps/syslog/ebin -pa ./ebin
Erlang/OTP 26 [erts-14.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Eshell V14.1 (press Ctrl+G to abort, type help(). for help)
1> application:ensure_all_started(syslog_erlang).
{ok,[syslog,syslog_erlang]}
2> gen_server:call(my_server, log_info).
ok

