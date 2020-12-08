#!/usr/bin/env bash

./compile.sh

cd src

erl -run project launch_scenario_2 -run init stop