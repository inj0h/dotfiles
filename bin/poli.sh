#!/bin/bash
#
# filename:         poli.sh
# description:
#                   Useful functions for a "polish"ed CLI experience.
#

# --------------
# specific *nix
# --------------

if [[ `uname` == 'Darwin' ]]; then
    catc() {
        # copy to clipboard
        cat $1 | pbcopy
    }

elif [[ `uname -s` == 'Linux' ]]; then
    catc() {
        # copy to clipboard
        cat $1 | xclip -selection clipboard
    }

fi

# ------------
# shared *nix
# ------------

catl() {
    # cat then pipe to less
    cat $1 | less -r
}

pless() {
    # pipe to less
    $1 | less
}

# ----
# git
# ----

gbv() {
    # view git branches in (neo)vim
    git branch | nvim -RM -
}

gitshove() {
    # when push comes to shove
    git add -A . && git commit -m "Autoupdate."
}

glv() {
    # view git log in (neo)vim
    git log | nvim -RM -
}

mustache() {
    # when we must stash
    git stash save $1
}

# ----
# etc
# ----

fuss() {
    # Now filtered. Find responsibly.
    FUSS=""

    for arg in $@; do
        FUSS=$FUSS"$arg"

        if [ $arg != ${@: -1} ]; then
            FUSS=$FUSS"|"
        fi
    done

    if [ "$FUSS" == "" ]; then
        FUSS=".git|target|node_modules"
    fi

    find . -type f | grep -vE $FUSS
}

# sdcv
if type sdcv >/dev/null 2>&1; then
    dic() {
        # lookup definition and pipe to less
        sdcv -n --color $1 | less -r
    }
fi
