# See LICENSE for licensing information.
.SILENT:

PROJECT = navipoint

# Options.
# -Werror

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars
ERLC_OPTS += +warn_unused_import +warn_unused_function +warn_bif_clash
ERLC_OPTS += +warn_unused_record +warn_deprecated_function +warn_obsolete_guard
ERLC_OPTS += +strict_validation +warn_export_vars +warn_exported_vars
ERLC_OPTS += +warn_missing_spec +warn_untyped_record +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
# CT_OPTS += -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl lager
# Also dialyze the tests.
# DIALYZER_OPTS := -I include test/uffda -Werror_handling -Wrace_conditions -Wunmatched_returns
# DIALYZER_OPTS += --src -r test

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


EDOC_DIRS := ["src"]
EDOC_OPTS := {preprocess, true}, {source_path, ${EDOC_DIRS}}, nopackages, {subpackages, true}

include erlang.mk

dialyze-filtered:
	dialyzer --no_native `$(call erlang,$(call filter_opts.erl,$(ERLC_OPTS)))` $(DIALYZER_DIRS) $(DIALYZER_OPTS) \
	| fgrep --invert-match --file .dialyzer.ignore

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s navipoint -config test/test.config

typer::
	typer $(DIALYZER_PLT) -r src
