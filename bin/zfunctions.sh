#!/bin/bash
#
# filename:         zfunc.sh
# description:
#                   Useful functions for a smooth CLI experience.
#
# ---------------------------------------------------------------------------- #

# cat then pipe to clipboard
if [[ `uname` == 'Darwin' ]]; then
    function catc { cat $1 | pbcopy }
else
    function catc { cat $1 | xclip -selection clipboard }
fi

# cat then pipe to less
function catl { cat $1 | less }

# pipe from clipboard
if [[ `uname` == 'Darwin' ]]; then
    function cp2 { pbcopy > $1 }
else
    function cp2 { xsel -b > $1 }
fi

# pipe to less
function pless { $1 | less }

# git
function gshove { git add -A . && git commit -m "Autoupdate." }
