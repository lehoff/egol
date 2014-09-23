PROJECT = egol

DEPS = edown gen_leader gproc

dep_gproc = git git://github.com/uwiger/gproc 0.3.1
dep_edown = git https://github.com/esl/edown.git master
dep_gen_leader = git https://github.com/abecciu/gen_leader_revival.git master

include erlang.mk

ERLC_OPTS = +debug_info 
