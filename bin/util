#!/bin/bash
#
# Filename:         poli.sh
# Description:
#                   Useful functions for a "polish"ed CLI experience.
#

#
# Etc
#
fuss() {
    # Only seems to work on Linux for some reason.
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
        # Lookup definition and pipe to less.
        sdcv -n --color $1 | less -r
    }
fi
