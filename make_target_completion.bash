#!/usr/bin/env bash

cmd="grep -oE '^[a-zA-Z0-9_-]+:([^=]|$)' | grep -v -e '^makefile' -e '^help' | sed 's/[:+ ?$]//' | sed 's/[:+ ?$]//'"
complete -W "\`make -qp | $cmd\`" make

