PROJECT = rabbitmq_server_release
PROJECT_DESCRIPTION = RabbitMQ Server

# Propagate PROJECT_VERSION (from the command line or environment) to
# other components. If PROJECT_VERSION is unset, then an empty variable
# is propagated and the default version will fallback to the default
# value from rabbitmq-components.mk.
export RABBITMQ_VERSION = $(PROJECT_VERSION)

# Release artifacts are put in $(PACKAGES_DIR).
PACKAGES_DIR ?= $(abspath PACKAGES)


# List of plugins to include in a RabbitMQ release.
include plugins.mk

DEPS = rabbit_common rabbit $(PLUGINS) $

DISABLE_DISTCLEAN = 1

include rabbitmq-components.mk
include erlang.mk

# --------------------------------------------------------------------
# Distribution.
# --------------------------------------------------------------------

.PHONY: source-dist clean-source-dist

SOURCE_DIST_BASE ?= rabbitmq-server
SOURCE_DIST_SUFFIXES ?= tar.xz
SOURCE_DIST ?= $(PACKAGES_DIR)/$(SOURCE_DIST_BASE)-$(PROJECT_VERSION)

# The first source distribution file is used by packages: if the archive
# type changes, you must update all packages' Makefile.
SOURCE_DIST_FILES = $(addprefix $(SOURCE_DIST).,$(SOURCE_DIST_SUFFIXES))

source_dist:
	$(info $$PACKAGES_DIR= $(PACKAGES_DIR))
	$(info $$SOURCE_DIST_BASE = $(SOURCE_DIST_BASE))
	$(info $$PROJECT_VERSION = $(PROJECT_VERSION))
	$(info $$SOURCE_DIST = $(SOURCE_DIST))
	$(info $$SOURCE_DIST_SUFFIXES = $(SOURCE_DIST_SUFFIXES))
	$(info $$SOURCE_DIST_FILES = $(SOURCE_DIST_FILES))


.PHONY: $(SOURCE_DIST_FILES)

source-dist: $(SOURCE_DIST_FILES)
	@:
RSYNC ?= rsync
RSYNC_V_0 =
RSYNC_V_1 = -v
RSYNC_V_2 = -v
RSYNC_V = $(RSYNC_V_$(V))
RSYNC_FLAGS += -a $(RSYNC_V)		\
	       --exclude '.sw?' --exclude '.*.sw?'	\
	       --exclude '*.beam'			\
	       --exclude '*.d'				\
	       --exclude '*.pyc'			\
	       --exclude '.git*'			\
	       --exclude '.hg*'				\
	       --exclude '.travis.yml*'			\
	       --exclude '.*.plt'			\
	       --exclude '$(notdir $(ERLANG_MK_TMP))'	\
	       --exclude '_build/'			\
	       --exclude '__pycache__/'			\
	       --exclude 'ci/'				\
	       --exclude 'cover/'			\
	       --exclude 'deps/'			\
	       --exclude 'doc/'				\
	       --exclude 'docker/'			\
	       --exclude 'ebin/'			\
	       --exclude 'erl_crash.dump'		\
	       --exclude 'escript/'			\
	       --exclude 'MnesiaCore.*'			\
	       --exclude '$(notdir $(DEPS_DIR))/'	\
	       --exclude 'hexer*'			\
	       --exclude 'logs/'			\
	       --exclude 'packaging'			\
	       --exclude 'PKG_*.md'			\
	       --exclude '/plugins/'			\
	       --include 'cli/plugins'			\
	       --exclude '$(notdir $(DIST_DIR))/'	\
	       --exclude 'test'				\
	       --exclude 'xrefr'			\
	       --exclude '/$(notdir $(PACKAGES_DIR))/'	\
	       --exclude '/PACKAGES/'			\
	       --exclude '/amqp_client/doc/'		\
	       --exclude '/amqp_client/rebar.config'	\
	       --exclude '/cowboy/doc/'			\
	       --exclude '/cowboy/examples/'		\
	       --exclude '/rabbit/escript/'		\
	       --exclude '/rabbitmq_amqp1_0/test/swiftmq/build/'\
	       --exclude '/rabbitmq_amqp1_0/test/swiftmq/swiftmq*'\
	       --exclude '/rabbitmq_cli/escript/'	\
	       --exclude '/rabbitmq_mqtt/test/build/'	\
	       --exclude '/rabbitmq_mqtt/test/test_client/'\
	       --exclude '/rabbitmq_trust_store/examples/'\
	       --exclude '/ranch/doc/'			\
	       --exclude '/ranch/examples/'		\
	       --exclude '/sockjs/examples/'		\
	       --exclude '/workflow_sources/'		\
	       --delete					\
	       --delete-excluded

TAR ?= tar
TAR_V_0 =
TAR_V_1 = -v
TAR_V_2 = -v
TAR_V = $(TAR_V_$(V))

GZIP ?= gzip
BZIP2 ?= bzip2
XZ ?= xz

ZIP ?= zip
ZIP_V_0 = -q
ZIP_V_1 =
ZIP_V_2 =
ZIP_V = $(ZIP_V_$(V))

.PHONY: $(SOURCE_DIST)
.PHONY: clean-source-dist distclean-packages clean-unpacked-source-dist

#$(SOURCE_DIST):
#	@echo "SOURCE_DIST"
$(SOURCE_DIST): $(ERLANG_MK_RECURSIVE_DEPS_LIST)
	@echo "SOURCE_DIST" 
	$(info $$SOURCE_DIST = $(SOURCE_DIST))
	$(info $$@ = $(@))
	$(verbose) mkdir -p $(dir $@)
	@echo "$(gen_verbose) $(RSYNC) $(RSYNC_FLAGS) ./ $@/"
	$(gen_verbose) $(RSYNC) $(RSYNC_FLAGS) ./ $@/
	$(verbose) echo "$(PROJECT_DESCRIPTION) $(PROJECT_VERSION)" > "$@/git-revisions.txt"
	$(verbose) echo "$(PROJECT) $$(git rev-parse HEAD) $$(git describe --tags --exact-match 2>/dev/null || git symbolic-ref -q --short HEAD)" >> "$@/git-revisions.txt"
	$(verbose) echo "$$(TZ= git --no-pager log -n 1 --format='%cd' --date='format-local:%Y%m%d%H%M.%S')" > "$@.git-times.txt"
	$(verbose) cat packaging/common/LICENSE.head > $@/LICENSE
	$(verbose) mkdir -p $@/deps/licensing
	# 对所有的 dep  做处理
	$(verbose) set -e; for dep in $$(cat $(ERLANG_MK_RECURSIVE_DEPS_LIST) | LC_COLLATE=C sort); do \
		$(RSYNC) $(RSYNC_FLAGS) \
		 $$dep \
		 $@/deps; \
		rm -f \
		 $@/deps/rabbit_common/rebar.config \
		 $@/deps/rabbit_common/rebar.lock; \
		if test -f $@/deps/$$(basename $$dep)/erlang.mk && \
		   test "$$(wc -l $@/deps/$$(basename $$dep)/erlang.mk | awk '{print $$1;}')" = "1" && \
		   grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk)$$" $@/deps/$$(basename $$dep)/erlang.mk; then \
			echo "include ../../erlang.mk" > $@/deps/$$(basename $$dep)/erlang.mk; \
		fi; \
		sed -E -i.bak "s|^[[:blank:]]*include[[:blank:]]+\.\./.*erlang.mk$$|include ../../erlang.mk|" \
		 $@/deps/$$(basename $$dep)/Makefile && \
		rm $@/deps/$$(basename $$dep)/Makefile.bak; \
		mix_exs=$@/deps/$$(basename $$dep)/mix.exs; \
		if test -f $$mix_exs; then \
			(cd $$(dirname "$$mix_exs") && \
			 (test -d $@/deps/.hex || env DEPS_DIR=$@/deps MIX_HOME=$@/deps/.mix HEX_HOME=$@/deps/.hex MIX_ENV=prod FILL_HEX_CACHE=yes mix local.hex --force) && \
			 env DEPS_DIR=$@/deps MIX_HOME=$@/deps/.mix HEX_HOME=$@/deps/.hex MIX_ENV=prod FILL_HEX_CACHE=yes mix deps.get --only prod && \
			 cp $(CURDIR)/mk/rabbitmq-mix.mk . && \
			 rm -rf _build deps); \
		fi; \
		if test -f "$$dep/license_info"; then \
			cp "$$dep/license_info" "$@/deps/licensing/license_info_$$(basename "$$dep")"; \
			cat "$$dep/license_info" >> $@/LICENSE; \
		fi; \
		find "$$dep" -maxdepth 1 -name 'LICENSE-*' -exec cp '{}' $@/deps/licensing \; ; \
		(cd $$dep; \
		 echo "$$(basename "$$dep") $$(git rev-parse HEAD) $$(git describe --tags --exact-match 2>/dev/null || git symbolic-ref -q --short HEAD)") \
		 >> "$@/git-revisions.txt"; \
		! test -d $$dep/.git || (cd $$dep; \
		 echo "$$(env TZ= git --no-pager log -n 1 --format='%cd' --date='format-local:%Y%m%d%H%M.%S')") \
		 >> "$@.git-times.txt"; \
	done
#	$(verbose) cat packaging/common/LICENSE.tail >> $@/LICENSE
#	$(verbose) find $@/deps/licensing -name 'LICENSE-*' -exec cp '{}' $@ \;
#	$(verbose) rm -rf $@/deps/licensing
#	$(verbose) for file in $$(find $@ -name '*.app.src'); do \
#		sed -E -i.bak \
#		  -e 's/[{]vsn[[:blank:]]*,[[:blank:]]*(""|"0.0.0")[[:blank:]]*}/{vsn, "$(PROJECT_VERSION)"}/' \
#		  -e 's/[{]broker_version_requirements[[:blank:]]*,[[:blank:]]*\[\][[:blank:]]*}/{broker_version_requirements, ["$(PROJECT_VERSION)"]}/' \
#		  $$file; \
#		rm $$file.bak; \
#	done
#	$(verbose) echo "PLUGINS := $(PLUGINS)" > $@/plugins.mk
## Remember the latest Git timestamp.
#	$(verbose) sort -r < "$@.git-times.txt" | head -n 1 > "$@.git-time.txt"
## Mix Hex component requires a cache file, otherwise it refuses to build
## offline... That cache is an ETS table with all the applications we
## depend on, plus some versioning informations and checksums. There
## are two problems with that: the table contains a date (`last_update`
## field) and `ets:tab2file()` produces a different file each time it's
## called.
##
## To make our source archive reproducible, we fix the time of the
## `last_update` field to the last Git commit and dump the content of the
## table as an Erlang term to a text file.
##
## The ETS file must be recreated before compiling RabbitMQ. See the
## `restore-hex-cache-ets-file` Make target.
#	$(verbose) $(call erlang,$(call dump_hex_cache_to_erl_term,$(call core_native_path,$@),$(call core_native_path,$@.git-time.txt)))
## Fix file timestamps to have reproducible source archives.
#	$(verbose) find $@ -print0 | xargs -0 touch -t "$$(cat "$@.git-time.txt")"
#	$(verbose) rm "$@.git-times.txt" "$@.git-time.txt"
#
#define dump_hex_cache_to_erl_term
#  In = "$(1)/deps/.hex/cache.ets",
#  Out = "$(1)/deps/.hex/cache.erl",
#  {ok, DateStr} = file:read_file("$(2)"),
#  {match, Date} = re:run(DateStr,
#    "^([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})\.([0-9]{2})",
#    [{capture, all_but_first, list}]),
#  [Year, Month, Day, Hour, Min, Sec] = [erlang:list_to_integer(V) || V <- Date],
#  {ok, Tab} = ets:file2tab(In),
#  true = ets:insert(Tab, {last_update, {{Year, Month, Day}, {Hour, Min, Sec}}}),
#  Props = [
#    Prop
#    || {Key, _} = Prop <- ets:info(Tab),
#    Key =:= name orelse
#    Key =:= type orelse
#    Key =:= protection orelse
#    Key =:= named_table orelse
#    Key =:= keypos orelse
#    Key =:= heir orelse
#    Key =:= read_concurrency orelse
#    Key =:= write_concurrency orelse
#    Key =:= compressed],
#  Entries = ets:tab2list(Tab),
#  ok = file:write_file(Out, io_lib:format("~w.~n~w.~n", [Props, Entries])),
#  ok = file:delete(In),
#  init:stop().
#endef
#
$(SOURCE_DIST).manifest: $(SOURCE_DIST)
	@echo "manifest: $(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) | LC_COLLATE=C sort > $@"
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) | LC_COLLATE=C sort > $@


## 这里定义 tar的一些选项
ifeq ($(shell tar --version | grep -c "GNU tar"),0)
    # Skip all flags if this is Darwin (a.k.a. macOS, a.k.a. OS X)
    ifeq ($(shell uname | grep -c "Darwin"),0)
    TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS = --uid 0 \
    				    --gid 0 \
    				    --numeric-owner \
    				    --no-acls \
    				    --no-fflags \
    				    --no-xattrs
    endif
else
    TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS = --owner 0 \
    				    --group 0 \
    				    --numeric-owner
endif

$(SOURCE_DIST).tar.gz: $(SOURCE_DIST).manifest
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		$(TAR) $(TAR_V) $(TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS) --no-recursion -T $(SOURCE_DIST).manifest -cf - | \
		$(GZIP) --best > $@)

$(SOURCE_DIST).tar.bz2: $(SOURCE_DIST).manifest
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		$(TAR) $(TAR_V) $(TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS) --no-recursion -T $(SOURCE_DIST).manifest -cf - | \
		$(BZIP2) > $@

$(SOURCE_DIST).tar.xz: $(SOURCE_DIST).manifest
	$(info $$SOURCE_DIST.tar.gz = $(SOURCE_DIST))
	$(info $$(dir $(SOURCE_DIST)) = $(dir $(SOURCE_DIST)))
	@echo "Here : $(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		$(TAR) $(TAR_V) $(TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS) --no-recursion -T $(SOURCE_DIST).manifest -cf - | \
		$(XZ) > $@"
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		$(TAR) $(TAR_V) $(TAR_FLAGS_FOR_REPRODUCIBLE_BUILDS) --no-recursion -T $(SOURCE_DIST).manifest -cf - | \
		$(XZ) > $@

$(SOURCE_DIST).zip: $(SOURCE_DIST).manifest
	$(verbose) rm -f $@
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		$(ZIP) $(ZIP_V) --names-stdin $@ < $(SOURCE_DIST).manifest

#clean:: clean-source-dist
#
#clean-source-dist:
#	$(gen_verbose) rm -rf -- $(SOURCE_DIST_BASE)-*
#
#distclean:: distclean-packages
#
#distclean-packages:
#	$(gen_verbose) rm -rf -- $(PACKAGES_DIR)
#
### If a dependency doesn't have a clean target - do not call it
#clean-unpacked-source-dist:
#	for d in deps/*; do \
#		if test -f $$d/Makefile; then \
#			(! make -n clean) || (make -C $$d clean || exit $$?); \
#		fi; \
#	done
#
#clean-deps:
#	git clean -xfffd deps
#

# --------------------------------------------------------------------
# Packaging.
# --------------------------------------------------------------------

.PHONY: packages package-generic-unix

# This variable is exported so sub-make instances know where to find the
# archive.
PACKAGES_SOURCE_DIST_FILE ?= $(firstword $(SOURCE_DIST_FILES))

RABBITMQ_PACKAGING_TARGETS = package-deb package-rpm \
package-rpm-redhat package-rpm-fedora package-rpm-rhel6 package-rpm-rhel7 \
package-rpm-rhel8 package-rpm-suse package-rpm-opensuse package-rpm-sles11 \
package-windows

ifneq ($(filter $(RABBITMQ_PACKAGING_TARGETS),$(MAKECMDGOALS)),)
ifeq ($(RABBITMQ_PACKAGING_REPO),)
$(error Cannot find rabbitmq-packaging repository dir; please clone from rabbitmq/rabbitmq-packaging and specify RABBITMQ_PACKAGING_REPO)
endif
endif

$(RABBITMQ_PACKAGING_TARGETS): $(PACKAGES_SOURCE_DIST_FILE)
	$(verbose) $(MAKE) -C $(RABBITMQ_PACKAGING_REPO) $@ \
		SOURCE_DIST_FILE=$(abspath $(PACKAGES_SOURCE_DIST_FILE))

package-generic-unix: $(PACKAGES_SOURCE_DIST_FILE)
	@echo "$(verbose) $(MAKE) -C packaging $@  SOURCE_DIST_FILE=$(abspath $(PACKAGES_SOURCE_DIST_FILE))"
	$(verbose) $(MAKE) -C packaging $@  SOURCE_DIST_FILE=$(abspath $(PACKAGES_SOURCE_DIST_FILE))

packages: package-generic-unix





