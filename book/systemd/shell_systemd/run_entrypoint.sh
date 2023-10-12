#!/bin/bash

BASH_SCRIPT=helloworld.sh
SERVICE_SCRIPT=helloworld.service
sudo cp $BASH_SCRIPT /usr/local/bin/
sudo cp $SERVICE_SCRIPT /etc/systemd/system/

sudo systemctl stop $SERVICE_SCRIPT
sudo systemctl daemon-reload
sudo systemctl start $SERVICE_SCRIPT


