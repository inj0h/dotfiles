# ~/.zfunc
# ------------------------------------------------------------------------------

# cat then pipe to clipboard
if [ `uname` == 'Darwin' ]
then
    function catc { cat $1 | pbcopy }
else
    function catc { cat $1 | xclip -selection clipboard }
fi

# cat then pipe to less
function catl { cat $1 | less }

# pipe from clipboard
if [ `uname` == 'Darwin' ]
then
    function cpto { pbcopy > $1 }
else
    function cpto { xsel -b > $1 }
fi

# pipe to less
function pless { $1 | less }
