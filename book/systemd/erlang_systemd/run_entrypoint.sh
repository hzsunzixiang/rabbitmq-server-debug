#!/bin/bash


ERLANG_SCRIPT=helloworld.sh
SERVICE_SCRIPT=helloworld.service

sudo systemctl stop $SERVICE_SCRIPT

sudo cp $ERLANG_SCRIPT /usr/local/bin/
sudo cp $SERVICE_SCRIPT /etc/systemd/system/

sudo systemctl daemon-reload
sudo systemctl start $SERVICE_SCRIPT


