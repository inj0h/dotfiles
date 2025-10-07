#!/bin/bash


export PS1="\w>"

alias ec="emacsclient -n"
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log --graph"
alias groot='cd $(git rev-parse --show-toplevel)'
alias gs="git status"
alias gsroot='cd $(git rev-parse --show-superproject-working-tree)'
alias gundo='git reset HEAD~'
alias mflac='mfl'
alias tre="tree -aC -I '.git|node_modules|target|zig-cache' ."
alias u="cd .."
alias vscode="emacs &" # ;P
case "$(uname -s)" in
    "Darwin")
        alias chrome="open -a chromium --new --args -incognito"
        alias fx="open -a firefox"
        alias fxp="open -a firefox --new --args -private-window"
        alias get="pwd | pbcopy"
        alias here='cd $(pbpaste)'
        alias o="ls -AGo"
        alias uptime_plus="system_profiler SPSoftwareDataType -detailLevel mini"
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

case "$(uname -s)" in
    "Darwin")
        bash_comp_mac="/opt/homebrew/etc/profile.d/bash_completion.sh"
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
