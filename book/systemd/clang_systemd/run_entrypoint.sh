#!/bin/bash


make 
C_SCRIPT=helloworld
SERVICE_SCRIPT=helloworld.service

sudo systemctl stop $SERVICE_SCRIPT

sudo cp $C_SCRIPT /usr/local/bin/
sudo cp $SERVICE_SCRIPT /etc/systemd/system/

sudo systemctl daemon-reload
sudo systemctl start $SERVICE_SCRIPT


