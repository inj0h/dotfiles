# Filename:    lib.bash
# Description: Don't reinvent the wheel. Just put the wheels here.
#

#
# Variables
#
# Colors
SH_CLEAR="\[\033[0m\]"
SH_GREEN="\[\033[1;32m\]"
SH_RED="\[\033[1;31m\]"
ECHO_CLEAR='\033[0m'
ECHO_GREEN='\033[1;32m'
ECHO_RED='\033[1;31m'

# Git
GIT_STAT_CLEAN="nothing to commit, working tree clean"
GIT_SIGIL_DIRTY="X"

#
# Functions
#
# Excuse these pathetic fake types.
#
# dir_make :: String -> Maybe Directory
function dir_make {
    if [ ! -d $1 ]; then
        echo "Creating $1..."
        mkdir $1
        greenp "Done."
    else
        redp "Directory $1 already exists!"
    fi
}

# dot_connect :: String -> String -> String -> String -> SymbolicLink
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
    [[ $(git status 2> /dev/null | tail -n1) != "$GIT_STAT_CLEAN" &&
           $(git status 2> /dev/null | tail -n1) != "" ]] &&
        echo "$GIT_SIGIL_DIRTY"
}

# git_parse_branch :: String -> String
function git_parse_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

# echo_green :: String -> String
function echo_green {
    echo -e ${ECHO_GREEN}$1${ECHO_CLEAR}
    sleep 0.5
}

# echo_red :: String -> String
function echo_red {
    echo -e ${ECHO_RED}$1${ECHO_CLEAR}
    sleep 1
}
