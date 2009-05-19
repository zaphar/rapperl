all: compile

compile:
	erl -make | grep -v Warning

shell: compile
	erl -pa ebin
clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
