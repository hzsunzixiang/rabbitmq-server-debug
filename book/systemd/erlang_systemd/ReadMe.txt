

模拟RabbitMQ的代码结构: 最外层没有代码

wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap 


# 获取所有的 ebin
DIR_NAME=/home/ericksun/workspace/rabbitmq-server-debug/book/systemd/erlang_systemd
for i in $(find $DIR_NAME -name ebin); do echo -pa $i; done |tr '\n'  ' '
ericksun@centos7-mq1:~/program/rabbitmq-server-debug/book/erlang_systemd (main)$ for i in $(find $DIR_NAME -name ebin); do echo -pa $i; done |tr '\n'  ' '



[root@rabbitmq erlang_systemd]# systemctl status helloworld
● helloworld.service - HelloWorld Service
     Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; preset: disabled)
     Active: active (running) since Thu 2025-01-02 21:17:03 EST; 1s ago
   Main PID: 118031 (helloworld.sh)
     Status: "Processing  data"
      Tasks: 26 (limit: 22563)
     Memory: 42.3M
        CPU: 489ms
     CGroup: /system.slice/helloworld.service
             ├─118031 /bin/bash /usr/local/bin/helloworld.sh
             ├─118032 /home/ericksun/install/otp_src_26.2.5.1/lib/erlang/erts-14.2.5.1/bin/beam.smp -- -root /home/ericksun/install/otp_src_26.2.5.1/lib/erlang -bindir /home/ericksun/insta>
             └─118038 erl_child_setup 1024

Jan 02 21:16:58 rabbitmq systemd[1]: Starting HelloWorld Service...
Jan 02 21:16:58 rabbitmq helloworld.sh[118032]: Starting up ...
Jan 02 21:16:58 rabbitmq helloworld.sh[118032]: Startup complete before notify
Jan 02 21:17:03 rabbitmq helloworld.sh[118032]: Startup complete after notify
Jan 02 21:17:03 rabbitmq systemd[1]: Started HelloWorld Service.

[root@rabbitmq erlang_systemd]# systemctl status helloworld
● helloworld.service - HelloWorld Service
     Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; preset: disabled)
     Active: active (running) since Thu 2025-01-02 21:17:03 EST; 7s ago
   Main PID: 118031 (helloworld.sh)
     Status: "After Processing  data"
      Tasks: 26 (limit: 22563)
     Memory: 42.3M
        CPU: 489ms
     CGroup: /system.slice/helloworld.service
             ├─118031 /bin/bash /usr/local/bin/helloworld.sh
             ├─118032 /home/ericksun/install/otp_src_26.2.5.1/lib/erlang/erts-14.2.5.1/bin/beam.smp -- -root /home/ericksun/install/otp_src_26.2.5.1/lib/erlang -bindir /home/ericksun/insta>
             └─118038 erl_child_setup 1024

Jan 02 21:16:58 rabbitmq systemd[1]: Starting HelloWorld Service...
Jan 02 21:16:58 rabbitmq helloworld.sh[118032]: Starting up ...
Jan 02 21:16:58 rabbitmq helloworld.sh[118032]: Startup complete before notify
Jan 02 21:17:03 rabbitmq helloworld.sh[118032]: Startup complete after notify
Jan 02 21:17:03 rabbitmq systemd[1]: Started HelloWorld Service.
