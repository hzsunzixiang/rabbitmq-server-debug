


模拟RabbitMQ的代码结构: 最外层没有代码

wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap 


# 获取所有的 ebin
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ for i in $(find ~/program/rabbitmq-server-debug/book/erlang_systemd/   -name ebin); do echo -pa $i; done |tr '\n'  ' '


# 看到了现象
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: activating (start) since Thu 2023-10-05 05:56:31 EDT; 329ms ago
 Main PID: 45252 (beam.smp)
    Tasks: 25
   CGroup: /system.slice/helloworld.service
           ├─45252 /home/ericksun/install/otp-26.1/lib/erlang/erts-14.1/bin/beam.smp -- -root /home/ericksun/install/otp-26.1/lib/erlang -bindir /home/eric...
           └─45258 erl_child_setup 1024
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Thu 2023-10-05 05:56:36 EDT; 388ms ago
 Main PID: 45252 (beam.smp)
   Status: "Processing  data"
    Tasks: 25
   CGroup: /system.slice/helloworld.service
           ├─45252 /home/ericksun/install/otp-26.1/lib/erlang/erts-14.1/bin/beam.smp -- -root /home/ericksun/install/otp-26.1/lib/erlang -bindir /home/eric...
           └─45258 erl_child_setup 1024
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Thu 2023-10-05 05:56:36 EDT; 7s ago
 Main PID: 45252 (beam.smp)
   Status: "After Processing  data"
    Tasks: 25
   CGroup: /system.slice/helloworld.service
           ├─45252 /home/ericksun/install/otp-26.1/lib/erlang/erts-14.1/bin/beam.smp -- -root /home/ericksun/install/otp-26.1/lib/erlang -bindir /home/eric...
           └─45258 erl_child_setup 1024
