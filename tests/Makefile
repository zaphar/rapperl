all: compile

compile: clean
	erl -make | grep -v Warning

compile_show_warnings:
	erl -make

copy_beams:
	cp ../ebin/* ebin/
shell: copy_beams
	erl -pa ebin
clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
