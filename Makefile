ERL_SRCS  = $(wildcard src/*.erl)
ERL_HRLS  = $(wildcard include/*.hrl)
TEST_SRCS = $(wildcard tests/*.erl)

compile: $(ERL_SRCS) $(ERL_HRLS) $(TEST_SRCS)
	@erl -make | grep -v Warning

shell: compile
	@erl -pa ebin

clean:
	@rm -f ebin/*.beam
	@rm -f erl_crash.dump


check: compile
	@erl -noshell -pa ebin -s test_suite test -s init stop 
