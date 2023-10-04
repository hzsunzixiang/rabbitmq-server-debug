ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld                                                           [1/1876]
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: activating (start) since Wed 2023-10-04 07:24:02 EDT; 9s ago
 Main PID: 11390 (helloworld)
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─11390 /usr/local/bin/helloworld

Oct 04 07:24:02 centos7-mq1 systemd[1]: Starting HelloWorld Service...
ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 07:24:12 EDT; 4s ago
 Main PID: 11390 (helloworld)
   Status: "Processing  data"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─11390 /usr/local/bin/helloworld

Oct 04 07:24:02 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 07:24:12 centos7-mq1 systemd[1]: Started HelloWorld Service.
ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 07:24:12 EDT; 1min 2s ago
 Main PID: 11390 (helloworld)
   Status: "Processing  data"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─11390 /usr/local/bin/helloworld

Oct 04 07:24:02 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 07:24:12 centos7-mq1 systemd[1]: Started HelloWorld Service.

ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 07:24:12 EDT; 1min 7s ago
 Main PID: 11390 (helloworld)
   Status: "Waiting for data"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─11390 /usr/local/bin/helloworld

Oct 04 07:24:02 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 07:24:12 centos7-mq1 systemd[1]: Started HelloWorld Service.

