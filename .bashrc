#!/bin/bash

#
# 00. User Variables:
#

# Colors
co_clear="\033[0m"
co_red="\033[0;31m"

# Unicode
sy_kanji_kido="\u8d77\u52d5"
sy_kanji_guchi="\u53e3"
sy_kanji_iri="\u5165"

#
# 01. Startup & Prompt:
#

echo -e "${co_red}$sy_kanji_kido$sy_kanji_iri$sy_kanji_guchi${co_clear}"
echo -e "皆さん、今はターミナルドグマに入ります。| 여러분, 지금은 터미널 도그마에 들어갑니다."
export PS1="\w/>"

#
# 02. Aliases:
#

# Basic
alias ec="emacsclient -n"
alias o="ls -AGgho"
alias t="tree -aC -I '.git|node_modules|target' ."

# Git
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
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

#
# 03. Functions:
#

gr() {
    grep --color\
         --exclude-dir={.git,.idea,build,dist,node_modules,target}\
         --exclude 'Cargo.lock'\
         --exclude 'package-lock.json'\
         -Iinr "$1" .
}

#
# 04. Externalities:
#

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

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.config/.rgrc"
