#!/bin/bash

echo "Hello, World!"

mkfifo /tmp/helloworld.sock

systemd-notify --ready --status="Waiting for data …....."

while : ; do
        echo "...NOTIFY_SOCKET:[ $NOTIFY_SOCKET ]..." > /tmp/notify.txt
        read -r a < /tmp/helloworld.sock

        systemd-notify --status="Processing $a"   
 
		# Do something with $a …
		sleep 10
		echo "receive: $a"

        systemd-notify --status="Waiting for data…"
done
