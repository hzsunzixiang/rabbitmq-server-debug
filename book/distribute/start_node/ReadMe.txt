
需要先启动epmd

可以用 一个命令激活 erl -sname apple


Eshell V14.1 (press Ctrl+G to abort, type help(). for help)
1> observer:start().
ok
2> net_kernel:start(['rabbit@centos7-mq1', shortnames, 60*1000 div 4]).
{ok,<0.261.0>}
(rabbit@centos7-mq1)3>




