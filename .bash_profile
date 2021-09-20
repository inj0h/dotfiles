#!/bin/bash
# Filename: .bash_profile
# Note:     A good enough bash_profile.

# 00. Variables
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

HISTCONTROL=ignoreboth
HISTSIZE=100000

# 01. Source
[ -r ~/.bashrc ] && . ~/.bashrc
