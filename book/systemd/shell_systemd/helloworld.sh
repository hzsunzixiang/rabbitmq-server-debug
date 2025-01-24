#!/bin/bash

#sleep 5 
systemd-notify --ready --status="Waiting for data …....."
sleep 10

while : ; do
        echo "...NOTIFY_SOCKET:[ $NOTIFY_SOCKET ]..." > /tmp/notify.txt
        systemd-notify --status="do something hello....."   
	# Do something with
	sleep 5 
        systemd-notify --status="do something world....."   
	sleep 5 
done
