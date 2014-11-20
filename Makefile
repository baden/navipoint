# See LICENSE for licensing information.

PROJECT = navipoint

# Options.
# -Werror
ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
	+'{parse_transform, lager_transform}'
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = lager cowboy jsxn navidb navistats
# TEST_DEPS = ct_helper gun
TEST_DEPS = gun
# dep_ct_helper = git https://github.com/extend/ct_helper.git master

dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1
dep_navidb = git git://github.com/baden/navidb.git master
dep_navistats = git git://github.com/baden/navistats.git master

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s navipoint -config test/test.config
