#!/bin/bash


EDITOR="vi"
VISUAL="$EDITOR"
HISTCONTROL=ignoreboth
HISTSIZE=9000

case "$(uname -s)" in
    "Darwin")
        # Brew
        export PATH="/opt/homebrew/bin:$PATH"
        export PATH="/opt/homebrew/sbin:$PATH"
        ;;
    *)
        # N/A
esac

. "$HOME/.bashrc"
