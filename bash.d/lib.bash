# Filename:    lib.bash
# Description: Don't reinvent the wheel. Just put the wheels here.
#

#
# Variables
#
# Paths
DOTFILES="$HOME/dotfiles"
BIN="$DOTFILES/bin"

# Bash prompt colors (Bash CLear|GReen|Red)
BCL="\[\033[0m\]"
BGR="\[\033[1;32m\]"
BRD="\[\033[1;31m\]"

# Colors
CLEAR='\033[0m'
GREEN='\033[1;32m'
RED='\033[1;31m'

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

# greenp :: String -> String
function greenp {
    echo -e ${GREEN}$1${CLEAR}
    sleep 0.5
}

# redp :: String -> String
function redp {
    echo -e ${RED}$1${CLEAR}
    sleep 1
}

# parse_git_dirty :: String -> String
function parse_git_dirty {
    [[ $(git status 2> /dev/null | tail -n1) != "$GIT_STAT_CLEAN" &&
           $(git status 2> /dev/null | tail -n1) != "" ]] &&
        echo "$GIT_SIGIL_DIRTY"
}

# parse_git_branch :: String -> String
function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}
