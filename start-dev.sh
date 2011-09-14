#!/bin/sh

exec cerl -valgrind -pa ebin deps/*/ebin -boot start_sasl \
    -sname circa_dev +K true +A 10 \
    -setcookie blah
