# Copyright 2010 Bob.sh

all: src/stomp_lexer.erl src/stomp_parser.erl
	@mkdir -p ebin/
	@erl -make

	@erlc -Iebin -Isrc \
		-o ebin \
		src/*.erl

src/stomp_lexer.erl: src/stomp_lexer.xrl
	erl -noshell -noinput \
		-s leex file "src/stomp_lexer.xrl" -s erlang halt

src/stomp_parser.erl: src/stomp_parser.yrl
	erl -noshell -noinput \
		-s yecc file "src/stomp_parser.yrl" -s erlang halt

clean: 
	rm -f ebin/*.beam
	rm -f ebin/*.boot
	rm -f ebin/*.script
	rm -fr doc/api
	rm -f erl_crash*

test: all
	@erl	-pa ebin \
		-smp auto \
		-noshell \
		-noinput \
		-s stomp_alltests \
		start

doc:
	@mkdir -p doc/api/
	@erl 	-noshell \
		-run edoc_run \
		application "stomp" "src" '[{dir,"doc/api"}]' \
		-s init stop
