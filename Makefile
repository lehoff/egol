PROJECT = egol

DEPS = edown gen_leader gproc lager

dep_gproc = git git://github.com/uwiger/gproc 0.3.1
dep_edown = git https://github.com/esl/edown.git master
dep_gen_leader = git https://github.com/abecciu/gen_leader_revival.git master

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Use the same settings for compiling releases as well as for testing
ERLC_OPTS= $(ERLC_COMPILE_OPTS) +debug_info
TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)

release:
	relx --vm_args "./config/vm.args"


