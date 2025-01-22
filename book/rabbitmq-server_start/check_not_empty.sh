#!/bin/bash
set -x 
check_start_params() {
    check_not_empty RABBITMQ_BOOT_MODULE
    check_not_empty SASL_BOOT_FILE
}

check_not_empty() {
    local name="${1:?}"
    local value
    eval value=\$$name
    if [ -z "$value" ]; then
        echo "Error: ENV variable should be defined: $1.
       Please check rabbitmq-env, rabbitmq-defaults, and ${RABBITMQ_CONF_ENV_FILE} script files"
        exit 78
    fi
}


HELLO=world
check_not_empty HELLO 
check_not_empty WORLD

# 检查参数报错
check_not_empty 
#
#+ HELLO=world
#+ check_not_empty HELLO
#+ local name=HELLO
#+ local value
#+ eval 'value=$HELLO'
#++ value=world
#+ '[' -z world ']'
#+ check_not_empty WORLD
#+ local name=WORLD
#+ local value
#+ eval 'value=$WORLD'
#++ value=
#+ '[' -z '' ']'
#+ echo 'Error: ENV variable should be defined: WORLD.
#       Please check rabbitmq-env, rabbitmq-defaults, and  script files'
#Error: ENV variable should be defined: WORLD.
#       Please check rabbitmq-env, rabbitmq-defaults, and  script files
#+ exit 78


#
#+ set -x
#+ HELLO=world
#+ check_not_empty
#check_not_empty.sh: line 9: 1: parameter null or not set
