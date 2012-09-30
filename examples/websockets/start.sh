#!/bin/sh
erl -pa ebin ../../ebin ../../deps/*/ebin -boot start_sasl -s websockets \
	-eval "io:format(\"Point your browser at http://localhost:8080~n\")."
