
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
