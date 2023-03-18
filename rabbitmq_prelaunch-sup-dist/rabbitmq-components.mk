ifeq ($(.DEFAULT_GOAL),)
# Define default goal to `all` because this file defines some targets
# before the inclusion of erlang.mk leading to the wrong target becoming
# the default.
.DEFAULT_GOAL = all
endif

# PROJECT_VERSION defaults to:
#   1. the version exported by rabbitmq-server-release;
#   2. the version stored in `git-revisions.txt`, if it exists;
#   3. a version based on git-describe(1), if it is a Git clone;
#   4. 0.0.0

PROJECT_VERSION := $(RABBITMQ_VERSION)

ifeq ($(PROJECT_VERSION),)
PROJECT_VERSION := $(shell \
if test -f git-revisions.txt; then \
    head -n1 git-revisions.txt | \
    awk '{print $$$(words $(PROJECT_DESCRIPTION) version);}'; \
else \
    (git describe --dirty --abbrev=7 --tags --always --first-parent \
     2>/dev/null || echo rabbitmq_v0_0_0) | \
    sed -e 's/^rabbitmq_v//' -e 's/^v//' -e 's/_/./g' -e 's/-/+/' \
     -e 's/-/./g'; \
fi)
endif


# Third-party dependencies version pinning.
#
# We do that in this file, which is copied in all projects, to ensure
# all projects use the same versions. It avoids conflicts and makes it
# possible to work with rabbitmq-public-umbrella.

dep_ranch = hex 2.1.0
dep_recon = hex 2.5.2
dep_thoas = hex 0.4.0
dep_observer_cli = hex 1.7.3
dep_sysmon_handler = hex 1.3.0
dep_ra = hex 2.4.5


RABBITMQ_COMPONENTS = rabbit \
		      rabbit_common \
		      rabbitmq_cli \
		      rabbitmq_codegen \

# Erlang.mk does not rebuild dependencies by default, once they were
# compiled once, except for those listed in the `$(FORCE_REBUILD)`
# variable.
#
# We want all RabbitMQ components to always be rebuilt: this eases
# the work on several components at the same time.

FORCE_REBUILD = $(RABBITMQ_COMPONENTS)

# --------------------------------------------------------------------
# Umbrella-specific settings.
# --------------------------------------------------------------------

# If the top-level project is a RabbitMQ component, we override
# $(DEPS_DIR) for this project to point to the top-level's one.
#
# We also verify that the guessed DEPS_DIR is actually named `deps`,
# to rule out any situation where it is a coincidence that we found a
# `rabbitmq-components.mk` up upper directories.

possible_deps_dir_1 = $(abspath ..)
possible_deps_dir_2 = $(abspath ../../..)

ifeq ($(notdir $(possible_deps_dir_1)),deps)
ifneq ($(wildcard $(possible_deps_dir_1)/../rabbitmq-components.mk),)
deps_dir_overriden = 1
DEPS_DIR ?= $(possible_deps_dir_1)
DISABLE_DISTCLEAN = 1
endif
endif

ifeq ($(deps_dir_overriden),)
ifeq ($(notdir $(possible_deps_dir_2)),deps)
ifneq ($(wildcard $(possible_deps_dir_2)/../rabbitmq-components.mk),)
deps_dir_overriden = 1
DEPS_DIR ?= $(possible_deps_dir_2)
DISABLE_DISTCLEAN = 1
endif
endif
endif

ifneq ($(wildcard UMBRELLA.md),)
DISABLE_DISTCLEAN = 1
endif

# We disable `make distclean` so $(DEPS_DIR) is not accidentally removed.

ifeq ($(DISABLE_DISTCLEAN),1)
ifneq ($(filter distclean distclean-deps,$(MAKECMDGOALS)),)
SKIP_DEPS = 1
endif
endif
