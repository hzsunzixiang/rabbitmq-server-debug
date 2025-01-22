#!/bin/bash
#set -x 
rmq_realpath() {
    local path=$1

    if [ -d "$path" ]; then
        cd "$path" && pwd
    elif [ -f "$path" ]; then
        cd "$(dirname "$path")" && echo $(pwd)/$(basename "$path")
    else
        echo "$path"
    fi
}

RABBITMQ_HOME="$(rmq_realpath "..")"
echo $RABBITMQ_HOME

RABBITMQ_HOME="$(rmq_realpath "./rmq_realpath.sh")"
echo $RABBITMQ_HOME
