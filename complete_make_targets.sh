#!/usr/bin/env bash

complete -W "\`make -qp | grep -oE '^[a-zA-Z0-9_-]+:([^=]|$)' | grep -v -e 'makefile' | sed 's/[:$|: $]//'\`" make
