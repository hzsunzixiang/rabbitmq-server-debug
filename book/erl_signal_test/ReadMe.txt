

模拟RabbitMQ的代码结构: 最外层没有代码

wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap 

ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erl_signal_test (main)$ erl -pa ebin/
Erlang/OTP 26 [erts-14.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Eshell V14.1 (press Ctrl+G to abort, type help(). for help)
1>  observer:start().
ok
2> gen_event:which_handlers(erl_signal_server).
[erl_signal_handler]
3> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","5.1"},
 {kernel,"ERTS  CXC 138 10","9.1"}]
4> application:ensure_all_started(erl_signal_test).
{ok,[erl_signal_test]}
5> application:which_applications().
[{erl_signal_test,"New project","0.1.0"},
 {stdlib,"ERTS  CXC 138 10","5.1"},
 {kernel,"ERTS  CXC 138 10","9.1"}]
6> application:which_applications().
[{erl_signal_test,"New project","0.1.0"},
 {stdlib,"ERTS  CXC 138 10","5.1"},
 {kernel,"ERTS  CXC 138 10","9.1"}]
7> gen_event:which_handlers(erl_signal_server).
[rabbit_prelaunch_sighandler,erl_signal_handler]


ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book (main)$ kill -1 86959
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book (main)$ kill -2 86959
8>
BREAK: (a)bort (A)bort with dump (c)ontinue (p)roc info (i)nfo
       (l)oaded (v)ersion (k)ill (D)b-tables (d)istribution



# SIGUSR1

ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book (main)$ kill -10 86959

9> Received SIGUSR1


# kill -12 87219
2> User defined signal 2




Crash dump is being written to: erl_crash.dump...done





