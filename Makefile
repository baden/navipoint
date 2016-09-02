# See LICENSE for licensing information.
.SILENT:

PROJECT = navipoint

# Options.
# -Werror
# ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
# 	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
# 	+'{parse_transform, lager_transform}'

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
# CT_OPTS += -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = lager cowboy jsxn navidb navistats
# TEST_DEPS = ct_helper gun
TEST_DEPS = gun erlware_commons xref_runner
# dep_ct_helper = git https://github.com/extend/ct_helper.git master

#dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_cowboy = git git://github.com/baden/cowboy.git master
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1
dep_navidb = git git://github.com/baden/navidb.git master
dep_navistats = git git://github.com/baden/navistats.git master
dep_erlware_commons = git https://github.com/erlware/erlware_commons.git master

BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk
# ESCRIPT_NAME = fprof_totals_cli
# ESCRIPT_FILE = fproftotals

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s navipoint -config test/test.config
