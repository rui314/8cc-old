#!/bin/sh
echo
./tests
[ "`./hello`" = 'Hello, world!' ] || (echo NG; exit)
echo OK
