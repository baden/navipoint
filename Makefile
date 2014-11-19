# See LICENSE for licensing information.

PROJECT = navipoint

# Options.
# -Werror
ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
	+'{parse_transform, lager_transform}'
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl

# Dependencies.

# bear not got automaticaly by folsom, bug?
DEPS = lager cowboy jsxn folsom bear navidb
# TEST_DEPS = ct_helper gun
TEST_DEPS = gun
# dep_ct_helper = git https://github.com/extend/ct_helper.git master

dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1
# dep_folsom = git https://github.com/boundary/folsom.git master
dep_navidb = git git://github.com/baden/navidb.git master
# dep_meck = git git://github.com/eproxus/meck.git 0.8.2
dep_folsom = git https://github.com/baden/folsom.git patch-1
dep_bear = git git://github.com/baden/bear.git master

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test
