#!/bin/bash
# Filename: .bashrc
# Note:     A good enough bashrc.

# 00. Startup Message - Neon Genesis Shell Session
# Colors
co_clear="\033[0m"
co_red="\033[0;31m"

# Unicode
sy_kanji_kido="\u8d77\u52d5"
sy_quotel_ja="\u300e"
sy_quoter_ja="\u300f"

echo -e "${co_red}$sy_kanji_kido${co_clear}"
echo -e "\"Entering Terminal Dogma.\" | $sy_quotel_ja ターミナルドグマに入る。$sy_quoter_ja | \"터미널 도그마에 들어간다.\""


# 01. Preemption. I.e. stolen copypastas (mostly from Ubuntu defaults).
# If not running interactively, don't do anything.
case $- in
    *i*) ;;
    *) return;;
esac

# Append to the history file rather than overwriting it.
shopt -s histappend


# 02. Prompt
if command -v starship > /dev/null 2>&1;
then
    eval "$(starship init bash)"
else
    # I.e. When you can't download starship.
    # Colors
    co_clear="\033[0m"
    co_cyan="\033[0;36m"
    co_green="\033[0;32m"

    # Unicode
    sy_lambda="\u03bb"
    sy_print() {
        echo -en "$1"
    }

    export PS1="$co_cyan[$co_clear\W$co_cyan]$co_clear $co_green$(sy_print $sy_lambda):$co_clear "
fi


# 03. Aliases
# Basic
alias ec="emacsclient -n"
alias ff="find . -type f -iname"
alias ffd="find . -type d -iname"
alias o="ls -AGgho"
alias t="tree -aC ."
alias u="cd .."

# Git
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
alias gl="git log"
alias gs="git status"


# 04. Externalities
# Completion
case "$(uname -s)" in
    "Darwin")
        [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
        ;;
    "Linux")
        [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
        ;;
    *)
        echo "Error: Unlisted system detected. Deferring bash completion."
esac

# Starship Prompt
# Cf. 02. Prompt section.

export STARSHIP_CACHE=~/.config/starship/cache
export STARSHIP_CONFIG=~/.config/starship/config.toml
