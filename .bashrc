#!/bin/bash

### Exports and Aliases:

export PS1="\w>"

alias ec="emacsclient -n"
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
alias gL="git log"
alias gl="git --no-pager log | head -n 30"
alias groot='cd $(git rev-parse --show-toplevel)'
alias gs="git status"
alias gsroot='cd $(git rev-parse --show-superproject-working-tree)'
alias gt="git stash"
alias gundo='git reset HEAD~'
alias treee="tree -aC -I '.git|node_modules|target' ."
case "$(uname -s)" in
    "Darwin")
        alias go="pwd | pbcopy"
        alias here='cd $(pbpaste)'
        ;;
    "Linux")
        # NA
        ;;
    *)
        # NA
esac

### Functions:

gr() {
    grep --color\
         --exclude-dir={.git,.idea,build,dist,node_modules,target}\
         --exclude 'Cargo.lock'\
         --exclude 'package-lock.json'\
         -Iinr "$1" .
}

gT() {
    git stash show -p stash@{"$1"}
}

### (More) OS Specific Stuff:

case "$(uname -s)" in
    "Darwin")
        # Bash completion
        [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] &&
            . "/usr/local/etc/profile.d/bash_completion.sh"

        # Homebrew
        export PATH="/usr/local/sbin:$PATH"
        ;;
    "Linux")
        # Bash completion
        [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] &&
            . "/usr/local/etc/profile.d/bash_completion.sh"
        ;;
    *)
        echo "Error: Unlisted system detected. Deferring bash completion."
esac
