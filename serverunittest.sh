#!/usr/bin/env bash

./compile.sh

cd src

erl -run project unit_test -run init stop