#!/bin/bash

export PS1="\w>"

alias ec="emacsclient -n"
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gD="git --no-pager diff"
alias gd="git diff"
alias gL="git log"
alias gl="git --no-pager log | head -n 30"
alias groot='cd $(git rev-parse --show-toplevel)'
alias gs="git status"
alias gsroot='cd $(git rev-parse --show-superproject-working-tree)'
alias gt="git stash"
alias gtl="git stash list"
alias gtp="git stash pop"
alias gundo='git reset HEAD~'
alias treee="tree -aC -I '.git|node_modules|target' ."
alias u="cd .."
case "$(uname -s)" in
    "Darwin")
        alias o="ls -AGl"
        alias get="pwd | pbcopy"
        alias here='cd $(pbpaste)'
        ;;
    "Linux")
        # NA
        ;;
    *)
        # NA
esac

gr()
{
    grep --color\
         --exclude-dir={.git,.idea,build,dist,node_modules,target}\
         --exclude 'Cargo.lock'\
         --exclude 'package-lock.json'\
         -Iinr "$1" .
}

gta() { git stash apply "stash@{$1}"   ; }
gtc() { git stash clear                ; }
gtd() { git stash drop "stash@{$1}"    ; }
gts() { git stash show -p "stash@{$1}" ; }

case "$(uname -s)" in
    "Darwin")
        bash_comp_mac="/usr/local/etc/profile.d/bash_completion.sh"
        [[ -r "$bash_comp_mac" ]] && . "$bash_comp_mac"

        # For some tertiary Brew deps
        export PATH="/usr/local/sbin:$PATH"

        export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-17.jdk/Contents/Home"
        export PATH="/usr/local/opt/openjdk@17/bin:$PATH"
        ;;
    "Linux")
        # TODO() Update this path
        bash_comp_linux="/usr/local/etc/profile.d/bash_completion.sh"
        [[ -r "$bash_comp_linux" ]] && . "$bash_comp_linux"
        ;;
    *)
        echo "Error: Unlisted system detected. Deferring additional system software."
esac

. "$HOME/.cargo/env"
