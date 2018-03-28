# filename:         zshrc
# description:
#                   Personal zshell settings (a bit of a mess atm).
#
# ------------------------------------------------------------------------------

# ~/.zshrc
# Note! Vim indentation broken!
# ------------------------------------------------------------------------------

# Prompt and colors
# ------------------------------------------------------------------------------
autoload -U colors zsh-mime-setup select-word-style
colors                 # Colors.
zsh-mime-setup         # Run everything as if it's an executable.
select-word-style bash # Ctrl+W on words.

# VCS
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "[%{$fg[yellow]%}%s%{$reset_color%} %r\
 %{$fg[magenta]%}-> %b%{$reset_color%}]\
 %{$fg[yellow]%}%c%{$fg[red]%}%u%{$reset_color%}"

precmd() {  # Run before each prompt.
    vcs_info
}

setopt PROMPT_SUBST     # Allow for a more custom prompt.

PROMPT="[%{$fg[green]%}%n%{$reset_color%}] %~ \$vcs_info_msg_0_
%{$fg[green]%}%% %{$reset_color%}"

[ -z "$TMUX" ] && export TERM="xterm-256color"

export CLICOLOR=1

# LSCOLORS
if [[ `uname -s` == 'Linux' ]]; then
    export LS_COLORS='fi=00;00:di=00;32:ln=00;35:ex=00;31:so=00;34:pi=00;33'
else
    export LSCOLORS=cxfxdxgxbxegedabagacad
    # Order of lscolors from left to right.
    # directory
    # symlink
    # socket
    # pipe
    # executable
    # block special
    # char special
    # exec w/ setuid bit set
    # exec w/ setgid bit set
    # dir writable to others w/ sticky bit
    # dir writable to others w/o sticky bit
    #
    # a = black
    # b = red
    # c = green
    # d = yellow
    # e = blue
    # f = magenta
    # g = cyan
fi

# Keybindings
# Lookup in /etc/termcap or /etc/terminfo else, you can get the right keycode
# by typing ^v and then type the key or key combination you want to use.
# "man zshzle" for the list of available actions

# Completion, etc.
# ------------------------------------------------------------------------------
autoload -U compinit
compinit
zmodload -i zsh/complist
setopt hash_list_all            # Hash everything before completion.
setopt completealiases          # Complete aliases.
setopt always_to_end            # When completing from the middle of a word, move the cursor to the end of the word.
setopt complete_in_word         # Allow completion from within a word/phrase.
setopt correct                  # Spelling correction for commands.
setopt list_ambiguous           # Complete as much of a completion until it gets ambiguous.

zstyle ':completion::complete:*' use-cache on               # Completion caching, use rehash to clear.
zstyle ':completion:*' cache-path ~/.zsh/cache              # Cache path.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   # Ignore case.
zstyle ':completion:*' menu select=2                        # Menu if nb items > 2.
#zstyle ':completion:*' list-colors ${(s.:.)LSCOLORS}        # Colors!
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate # List of completors to use.

# Sections Completion
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format $'\e[00;32m%d'   # Color = green (term).
zstyle ':completion:*:messages' format $'\e[00;31m%d'       # Color = red (term).
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=32=37"  # Color = green, white (term).
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*' force-list always
zstyle ':completion:*' users $users
zstyle ':completion:*' special-dirs true
zstyle ':completion:*:(cd):*' ignored-patterns '.|..'       # Don't tab-comp ./ and ../ for cd

# Generic Completion with '--help'
compdef _gnu_generic gcc
compdef _gnu_generic gdb

# Pushd
setopt auto_pushd               # Make cd push old dir in dir stack.
setopt pushd_ignore_dups        # No duplicates in dir stack.
setopt pushd_silent             # No dir stack after pushd or popd.
setopt pushd_to_home            # `Pushd` = `pushd $HOME`.

# History
HISTFILE=~/.zsh_history         # Where to store zsh config.
HISTSIZE=1024                   # Big history.
SAVEHIST=1024                   # Big history.
setopt append_history           # Append.
setopt hist_ignore_all_dups     # No duplicate.
unsetopt hist_ignore_space      # Ignore space prefixed commands.
setopt hist_reduce_blanks       # Trim blanks.
setopt hist_verify              # Show before executing history commands.
setopt inc_append_history       # Add commands as they are typed, don't wait until shell exit.
setopt share_history            # Share hist between sessions.
setopt bang_hist                # !keyword.

# Various
setopt auto_cd                  # If command is a path, cd into it.
setopt auto_remove_slash        # Self explicit.
setopt chase_links              # Resolve symlinks.
setopt clobber                  # Truncate existing files when using >.
setopt correct                  # Try to correct spelling of commands.
setopt extended_glob            # Activate complex pattern globbing.
setopt glob_dots                # Include dotfiles in globbing.
setopt print_exit_value         # Print return value if non-zero.
unsetopt beep                   # No bell on error.
unsetopt bg_nice                # No lower prio for background jobs.
unsetopt hist_beep              # No bell on error in history.
unsetopt hup                    # No hup signal at shell exit.
unsetopt ignore_eof             # Do not exit on end-of-file.
unsetopt list_beep              # No bell on ambiguous completion.
unsetopt rm_star_silent         # Ask for confirmation for `rm *' or `rm path/*'.
# setxkbmap -option compose:ralt  # compose-key... shell doesn't seem to like this.

# Variables
# ------------------------------------------------------------------------------
# heresy
set -o emacs

# Scripts, etc
export PATH=$PATH:$HOME/bin

# Linux
# Remap capslock to control.
if [[ `uname` == 'Linux' ]]; then
    setxkbmap -option caps:ctrl_modifier
fi

# fzf colors
export FZF_DEFAULT_OPTS='
--color fg:252,bg:-1,hl:144,fg+:252,bg+:-1,hl+:228
--color info:144,prompt:114,spinner:-1,pointer:114,marker:193'

# fzf search
# export FZF_DEFAULT_COMMAND='ag --hidden --silent --ignore .git -f -g ""'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
# grep (w/ color)
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Colored man pages!
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

# Source files
# ------------------------------------------------------------------------------
# Source my aliases.
if [ -f ~/bin/alias.sh ]; then
    source ~/bin/alias.sh
fi

# Source my paths.
if [ -f ~/bin/path.sh ]; then
    source ~/bin/path.sh
fi

# Source my proxies.
if [ -f ~/bin/proxy.sh ]; then
    source ~/bin/proxy.sh
fi

# Functions
if [ -f ~/bin/zfunctions.sh ]; then
    source ~/bin/zfunctions.sh
fi

# What?
if [ -f ~/bin/secret.sh ]; then
    source ~/bin/secret.sh
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion