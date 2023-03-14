PROJECT = rabbitmq_server_release
PROJECT_DESCRIPTION = RabbitMQ Server

# Propagate PROJECT_VERSION (from the command line or environment) to
# other components. If PROJECT_VERSION is unset, then an empty variable
# is propagated and the default version will fallback to the default
# value from rabbitmq-components.mk.
export RABBITMQ_VERSION = $(PROJECT_VERSION)

# List of plugins to include in a RabbitMQ release.
include plugins.mk

DEPS = rabbit_common rabbit $(PLUGINS) $

DISABLE_DISTCLEAN = 1

include rabbitmq-components.mk
include erlang.mk
