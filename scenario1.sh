#!/usr/bin/env bash

./compile.sh

cd src

erl -run project launch_scenario_1 -run init stop