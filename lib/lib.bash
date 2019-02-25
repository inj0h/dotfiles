# Filename:    lib.bash
# Description: Don't reinvent the wheel. Just put the wheels here.
#

#
# Variables
#
# Colors
sh_clear="\[\033[0m\]"
sh_green="\[\033[1;32m\]"
sh_red="\[\033[1;31m\]"
echo_clear='\033[0m'
echo_green='\033[1;32m'
echo_red='\033[1;31m'

# Git
git_stat_clean="nothing to commit, working tree clean"
git_sigil_dirty="X"

#
# Functions
#
# Excuse these pathetic fake types.
#
# dir_make :: String -> Maybe Directory
function dir_make {
    [ "$#" -eq 1 ] && [ ! -d $1 ] && mkdir -v $1
}

# dot_connect :: String -> [String] -> String -> String -> SymbolicLink
function dot_connect {
    for file in $2; do
        if [ "$4" == "dot" ]; then
            ln $1 $file $3/.`basename $file`
        else
            ln $1 $file $3/`basename $file`
        fi
    done
}

# git_parse_dirty :: String -> String
function git_parse_dirty {
    [[ $(git status 2> /dev/null | tail -n1) != "$git_stat_clean" &&
           $(git status 2> /dev/null | tail -n1) != "" ]] &&
        echo "$git_sigil_dirty"
}

# git_parse_branch :: String -> String
function git_parse_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

# echo_green :: String -> String
function echo_green {
    echo -e ${echo_green}$1${echo_clear}
    sleep 0.5
}

# echo_red :: String -> String
function echo_red {
    echo -e ${echo_red}$1${echo_clear}
    sleep 1
}
