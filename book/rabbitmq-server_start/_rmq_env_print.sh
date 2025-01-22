#!/bin/bash

_rmq_env_now()
{
	    date '+%Y-%m-%d %H:%M:%S'
}

_rmq_env_print()
{
    _rmq_env_tmp="$1"
    _rmq_env_tmp_len="${#_rmq_env_tmp}"
    shift
    printf '%s %s %s\n' "$(_rmq_env_now)" "$_rmq_env_tmp" "$1" 1>&2
    shift
    _rmq_env_print_line=''
    _rmq_env_indent="$((_rmq_env_tmp_len + 21))"
    for _rmq_env_print_line in "$@"
    do
        printf "%${_rmq_env_indent}s%s\n" ' ' "$_rmq_env_print_line" 1>&2
    done
    unset _rmq_env_print_line
    unset _rmq_env_indent
    unset _rmq_env_tmp_len
    unset _rmq_env_tmp
}


_rmq_env_perr()
{
	    _rmq_env_print '[error]' "$@"
}

_rmq_env_pwarn()
{
	    _rmq_env_print '[warning]' "$@"
}

_rmq_env_print '[error]' "$@"

_rmq_env_pwarn 'RABBITMQ_PID_FILE was already set by the init script to:' \
        "saved_RABBITMQ_PID_FILE" \
        'The value set in rabbitmq-env.conf is ignored because it would break the init script.'



#[ericksun@rabbitmq4-1:~/workspace/rabbitmq-server-debug/book/rabbitmq-server_start] (main *%)$ bash _rmq_env_print.sh  xxx yy zz
#2025-01-21 21:10:00 [error] xxx
#                            yy
#                            zz
#2025-01-21 21:10:00 [warning] RABBITMQ_PID_FILE was already set by the init script to:
#                              saved_RABBITMQ_PID_FILE
#                              The value set in rabbitmq-env.conf is ignored because it would break the init script.
