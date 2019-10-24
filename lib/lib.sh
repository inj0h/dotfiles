#!/bin/sh
#
# Filename: lib.sh
# Note:     Don't reinvent the wheel. Just put the wheels here.
#

#
# Variables
#
# Colors
sh_blue="\[\033[0;34m\]"
sh_clear="\[\033[0m\]"
sh_cyan="\[\033[0;36m\]"
sh_green="\[\033[0;32m\]"
sh_purple="\[\033[0;35m\]"
sh_red="\[\033[0;31m\]"

echo_clear='\033[0m'
echo_green='\033[1;32m'
echo_red='\033[1;31m'

# Git
git_stat_clean="nothing to commit, working tree clean"

#
# Functions
#
# Excuse these pathetic fake types.
#
# dot_connect :: String -> [String] -> String -> SymbolicLink
dot_connect() {
    for file in $2; do
        ln "$1" "$file" "$3"/"$(basename "$file")"
    done
}

# git_parse_branch :: Exit -> String -> String
git_parse_branch() {
    git_status_check &&
        git branch --no-color 2> /dev/null |
            sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}

# git_parse_dirty :: Exit -> Exit -> Exit -> String
git_parse_dirty() {
    git_status_check &&
        git status 2> /dev/null |
                       tail -n1 | grep "$git_stat_clean" > /dev/null 2>&1
    [ $? = 1 ] && u_x
}

# git_parse_repo :: Exit -> String -> String
git_parse_repo() {
    git_status_check &&
        basename "$(git rev-parse --show-toplevel)"
}

# git_status_check :: String -> Exit
git_status_check() {
    git rev-parse --is-inside-work-tree > /dev/null 2>&1
}

# git_status_display :: Exit -> String -> String -> String
git_status_display() {
    git_status_check &&
        echo "[$(git_parse_repo) $(u_arrow_ltr) $(git_parse_branch)]"
}

# echo_green :: String -> String
echo_green() {
    echo "${echo_green}""$1""${echo_clear}"
    sleep 0.5
}

# echo_red :: String -> String
echo_red() {
    echo "${echo_red}""$1""${echo_clear}"
    sleep 1
}

# u_arrow_ltr :: String -> String
u_arrow_ltr() {
    printf "\u21fe"
}

# u_lambda :: String -> String
u_lambda() {
    printf "\u03bb"
}

# u_x :: String -> String
u_x() {
    printf "\u2717"
}
