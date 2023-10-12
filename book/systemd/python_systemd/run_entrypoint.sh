#!/bin/bash

PYTHON_SCRIPT=helloworld.py
SERVICE_SCRIPT=helloworld.service
sudo cp $PYTHON_SCRIPT /usr/local/bin/
sudo cp $SERVICE_SCRIPT /etc/systemd/system/

sudo systemctl stop $SERVICE_SCRIPT
sudo systemctl daemon-reload
sudo systemctl start $SERVICE_SCRIPT


