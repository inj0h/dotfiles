#!/bin/bash
# Filename: .bashrc
# Note:     A good enough bashrc.

# 00. Shared
# Colors
co_clear="\033[0m"
co_cyan="\033[0;36m"
co_green="\033[0;32m"
co_red="\033[0;31m"

# Unicode
sy_kanji_kido="\u8d77\u52d5" # start
sy_kanji_kuchi="\u53e3"      # mouth
sy_kanji_iri="\u5165"        # entrance
sy_lambda="\u03bb"
sy_quotel_ja="\u300e"
sy_quoter_ja="\u300f"

# 01. Startup Message - Neon Genesis Shell Session
echo -e "${co_red}$sy_kanji_kido$sy_kanji_iri$sy_kanji_kuchi${co_clear}"
echo -e "\"Entering Terminal Dogma.\" | $sy_quotel_ja ターミナルドグマに入る。$sy_quoter_ja | \"터미널 도그마에 들어간다.\""

# 02. Preemption (I.e. Stolen Ubuntu Defaults)
# If not running interactively, don't do anything.
case $- in
    *i*) ;;
    *) return;;
esac

# Append to the history file rather than overwriting it.
shopt -s histappend

# 03. Prompt
sy_print() {
    echo -en "$1"
}

export PS1="\[$co_cyan\][\[$co_clear\]\w\[$co_cyan\]]\[$co_clear\]\[$co_green\]$(sy_print $sy_lambda):\[$co_clear\]"

# 04. Aliases
# Basic
alias ec="emacsclient -n"
alias o="ls -AGgho"
alias t="tree -aC -I '.git|node_modules|target' ."
alias u="cd .."

# Git
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
alias ge="git stash"
alias gl="git log"
alias groot='cd $(git rev-parse --show-toplevel)'
alias gs="git status"
alias gsroot='cd $(git rev-parse --show-superproject-working-tree)'

# Platform-Specific
case "$(uname -s)" in
    "Darwin")
        alias go="pwd | pbcopy"
        alias here='cd $(pbpaste)'
        ;;
    "Linux")
        # N/A.
        ;;
    *)
        # N/A.
esac

# 05. Functions
d2() {
    # Basically an alias for piping two files from diff to diffr but passing
    # some flags to the latter to improve visibility.
    diff -u "$1" "$2" | diffr\
                            --colors refine-removed:foreground:black\
                            --colors refine-added:foreground:black
}

# 06. Externalities
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
