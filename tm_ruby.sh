#!/bin/sh
source /usr/local/share/chruby/chruby.sh
chruby ruby

exec ruby "$@"