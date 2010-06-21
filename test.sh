#!/bin/sh
./tests
[ "`./hello`" = 'Hello, world!' ] || (echo NG; exit)
echo OK
