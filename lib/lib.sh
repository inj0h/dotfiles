#!/bin/sh
#
# Filename: lib.sh
# Note:     Don't reinvent the wheel. Just put the wheels here.
#

#
# Functions
#
# Excuse these pathetic fake types.

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
