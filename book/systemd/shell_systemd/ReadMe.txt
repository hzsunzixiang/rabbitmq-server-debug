https://www.fosslinux.com/111815/a-guide-to-creating-linux-services-with-systemd.htm

ericksun@centos7-mq1:~$ sudo systemctl status helloworld.service                                                                                        [5/44]
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 06:22:52 EDT; 7s ago
 Main PID: 7039 (helloworld.sh)
   Status: "Waiting for data …....."
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─7039 /bin/bash /usr/local/bin/helloworld.sh

Oct 04 06:22:52 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: Hello, World!
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: mkfifo: cannot create fifo ‘/tmp/helloworld.sock’: File exists
Oct 04 06:22:52 centos7-mq1 systemd[1]: Started HelloWorld Service.

[root@centos7-mq1 ericksun]# echo "helloworld" > /tmp/helloworld.sock
[root@centos7-mq1 ericksun]# sudo systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 06:22:52 EDT; 1min 18s ago
 Main PID: 7039 (helloworld.sh)
   Status: "Processing helloworld"
    Tasks: 2
   CGroup: /system.slice/helloworld.service
           ├─7039 /bin/bash /usr/local/bin/helloworld.sh
           └─7108 sleep 10

Oct 04 06:22:52 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: Hello, World!
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: mkfifo: cannot create fifo ‘/tmp/helloworld.sock’: File exists
Oct 04 06:22:52 centos7-mq1 systemd[1]: Started HelloWorld Service.

[root@centos7-mq1 ericksun]# sudo systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled; vendor preset: disabled)
   Active: active (running) since Wed 2023-10-04 06:22:52 EDT; 1min 32s ago
 Main PID: 7039 (helloworld.sh)
   Status: "Waiting for data…"
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─7039 /bin/bash /usr/local/bin/helloworld.sh

Oct 04 06:22:52 centos7-mq1 systemd[1]: Starting HelloWorld Service...
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: Hello, World!
Oct 04 06:22:52 centos7-mq1 helloworld.sh[7039]: mkfifo: cannot create fifo ‘/tmp/helloworld.sock’: File exists
Oct 04 06:22:52 centos7-mq1 systemd[1]: Started HelloWorld Service.
Oct 04 06:24:17 centos7-mq1 helloworld.sh[7039]: receive: helloworld

