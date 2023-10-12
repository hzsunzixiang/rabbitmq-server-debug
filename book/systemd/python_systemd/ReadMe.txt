
ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: activating (start) since Wed 2023-10-04 07:04:04 EDT; 9s ago
 Main PID: 10273 (python)
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─10273 /usr/bin/python /usr/local/bin/helloworld.py

Oct 04 07:04:04 centos7-mq1 systemd[1]: Starting HelloWorld Service...
ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 07:04:14 EDT; 927ms ago
 Main PID: 10273 (python)
   Status: "Processing  data"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─10273 /usr/bin/python /usr/local/bin/helloworld.py

Oct 04 07:04:04 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 07:04:14 centos7-mq1 systemd[1]: Started HelloWorld Service.

ericksun@centos7-mq1:~/programming/linux/systemd (master)$ sudo systemctl status helloworld
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 07:04:14 EDT; 6s ago
 Main PID: 10273 (python)
   Status: "Waiting for data"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─10273 /usr/bin/python /usr/local/bin/helloworld.py

Oct 04 07:04:04 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 07:04:14 centos7-mq1 systemd[1]: Started HelloWorld Service.

