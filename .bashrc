# Filename: bashrc
# Note:     A good enough bashrc.

#
# Preemption
#

# Stolen from Ubuntu defaults:
# If not running interactively, don't do anything.
case $- in
    *i*) ;;
    *) return;;
esac

# Stolen from Ubuntu defaults:
# Append to the history file rather than overwriting it.
shopt -s histappend

#
# Variables
#

# Colors
#
# Shell prompt colors. I forget what the escaped bracket means.
sh_blue="\[\033[0;34m\]"
sh_clear="\[\033[0m\]"
sh_cyan="\[\033[0;36m\]"
sh_green="\[\033[0;32m\]"
sh_purple="\[\033[0;35m\]"
sh_red="\[\033[0;31m\]"

# Unicode
utf8_arrow_ltr="\u21fe"
utf8_checkmark="\u2713"
utf8_cross="\u2717"
utf8_lambda="\u03bb"

#
# Functions
#

# Print UTF-8 symbols.

utf8_print_arrow_ltr() {
    # This function prints the UTF-8 symbol for a left-to-right arrow.
    echo -en "$utf8_arrow_ltr"
}

utf8_print_checkmark() {
    # This function prints the UTF-8 symbol for a checkmark.
    echo -en "$utf8_checkmark"
}

utf8_print_cross() {
    # This function prints the UTF-8 symbol for a cross, or X.
    echo -en "$utf8_cross"
}

utf8_print_lambda() {
    # This function prints the UTF-8 symbol for a Greek lambda.
    echo -en "$utf8_lambda"
}

# Print Git status.

git_parse_branch() {
    # This function prints the branch name of the current working Git
    # repository.

    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        git branch --no-color 2> /dev/null |
            sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
    fi
}

git_parse_cache() {
    # This function prints a Unicode checkmark if the current working
    # Git repository has any staged changes.

    git_status_cache="Changes to be committed:"

    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        [ "$(git status | grep "$git_status_cache")" == "$git_status_cache" ] &&
            utf8_print_checkmark
    fi
}

git_parse_dirty() {
    # This function prints a Unicode cross or X if the current working
    # Git repository has any unstaged changes.

    git_status_dirty="Changes not staged for commit:"

    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        [ "$(git status | grep "$git_status_dirty")" == "$git_status_dirty" ] &&
            utf8_print_cross
    fi
}

git_parse_repo() {
    # This function prints the name of the current working Git
    # repository.

    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        basename "$(git rev-parse --show-toplevel)"
    fi
}

git_status_branch() {
    # This function prints the name of the current working Git
    # repository and its branch delimited by a Unicode arrow pointing
    # left-to-right.

    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
    then
        echo "[$(git_parse_repo) $(utf8_print_arrow_ltr) $(git_parse_branch)]"
    fi
}

#
# Prompt
#

export PS1="$sh_green\u$sh_clear $sh_cyan\$(pwd -P)$sh_clear $sh_blue\$(git_status_branch)$sh_clear $sh_red\$(git_parse_dirty)$sh_clear$sh_green\$(git_parse_cache)$sh_clear\n$sh_green\$(utf8_print_lambda)$sh_clear "

#
# Externalities
#

# Bash Completion
case "$(uname -s)" in
    "Darwin")
        bash_completion_darwin="/usr/local/etc/bash_completion"

        [ -r "$bash_completion_darwin" ] &&
            bash_completion="$bash_completion_darwin"
        ;;
    "Linux")
        bash_completion_linux="/etc/profile.d/bash_completion.sh"

        [ -r "$bash_completion_linux" ] &&
            bash_completion="$bash_completion_linux"
        ;;
    *)
        echo "Error: Unable to load bash completion from disk."
esac

[ -n "$bash_completion" ] && . "$bash_completion"

# Node Version Manager
#
# TODO: Configure NVM for Linux and Windows too.
case "$(uname -s)" in
    "Darwin")
        if command -v brew > /dev/null
        then
            if [ "$(brew list | grep nvm)" == "nvm" ]
            then
                nvm="/usr/local/opt/nvm/nvm.sh"
                nvm_bash_completion="/usr/local/opt/nvm/etc/bash_completion"
                nvm_directory="$HOME/.nvm"

                [ -s "$nvm" ] && . "$nvm"
                [ -s "$nvm_bash_completion" ] && . "$nvm_bash_completion"
                [ ! -d "$nvm_directory" ] && mkdir "$nvm_directory"

                export NVM_DIR="$nvm_directory"
            fi
        fi
        ;;
    *)
        echo "Error: Unable to load node version manager from disk."
esac
