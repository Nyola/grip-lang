#!/bin/bash

SCRIPT_PATH="${BASH_SOURCE[0]}";

if [ -h "${SCRIPT_PATH}" ]; then
  while [ -h "${SCRIPT_PATH}" ]; do SCRIPT_PATH=`readlink "${SCRIPT_PATH}"`; done
fi

cd `dirname ${SCRIPT_PATH}`

if [ ! -d ".tup" ]; then
  tup init
fi

tup upd
