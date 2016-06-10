# ~/.zshrc
# ------------------------------------------------------------------------------ 
autoload -U colors zsh-mime-setup select-word-style
colors          # colors
zsh-mime-setup  # run everything as if it's an executable
select-word-style bash # ctrl+w on words

# vcs 
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "%{$fg[yellow]%}%c%{$fg[green]%}%u%{$reset_color%} [%{$fg[blue]%}%b%{$reset_color%}] %{$fg[yellow]%}%s%{$reset_color%}:%r"
precmd() {  # run before each prompt
    vcs_info
}

# prompt
setopt PROMPT_SUBST     # allow funky stuff in prompt

color="blue"
if [ "$USER" = "root" ]; then
    color="red"         # root is red, user is blue
fi;

PROMPT="%{$fg[$color]%}%n%{$reset_color%}@%U%{$fg[yellow]%}%m%{$reset_color%}%u %B%~%b \$vcs_info_msg_0_
%{$fg[$color]%}%#%{$reset_color%} %{$fg[green]%}$ %{$reset_color%}"

# ls colors
export LSCOLORS=exfxdxbxcxegedabagacad                                  
# order of lscolors from left to right..
# directory = b
# symlink = f
# socket = d
# pipe = b
# executable = c
# b = red 
# c = green
# d = yellow 
# e = blue
# f = magenta
# g = cyan

# key bindings
# Lookup in /etc/termcap or /etc/terminfo else, you can get the right keycode
# by typing ^v and then type the key or key combination you want to use.
# "man zshzle" for the list of available actions

# a lot of stuff that I'm not quite sure of but used anyway 
# ------------------------------------------------------------------------------ 
# completion
autoload -U compinit
compinit
zmodload -i zsh/complist        
setopt hash_list_all            # hash everything before completion
setopt completealiases          # complete alisases
setopt always_to_end            # when completing from the middle of a word, move the cursor to the end of the word    
setopt complete_in_word         # allow completion from within a word/phrase
setopt correct                  # spelling correction for commands
setopt list_ambiguous           # complete as much of a completion until it gets ambiguous.

zstyle ':completion::complete:*' use-cache on               # completion caching, use rehash to clear
zstyle ':completion:*' cache-path ~/.zsh/cache              # cache path
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   # ignore case
zstyle ':completion:*' menu select=2                        # menu if nb items > 2
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}       # colorz !
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate # list of completers to use

# sections completion
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format $'\e[00;34m%d'
zstyle ':completion:*:messages' format $'\e[00;31m%d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true

zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=29=34"
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*' force-list always
# users=(jvoisin root)           # because I don't care about others
zstyle ':completion:*' users $users
zstyle ':completion:*' special-dirs true

# generic completion with --help
compdef _gnu_generic gcc
compdef _gnu_generic gdb

# pushd
setopt auto_pushd               # make cd push old dir in dir stack
setopt pushd_ignore_dups        # no duplicates in dir stack
setopt pushd_silent             # no dir stack after pushd or popd
setopt pushd_to_home            # `pushd` = `pushd $HOME`

# history
HISTFILE=~/.zsh_history         # where to store zsh config
HISTSIZE=1024                   # big history
SAVEHIST=1024                   # big history
setopt append_history           # append
setopt hist_ignore_all_dups     # no duplicate
unsetopt hist_ignore_space      # ignore space prefixed commands
setopt hist_reduce_blanks       # trim blanks
setopt hist_verify              # show before executing history commands
setopt inc_append_history       # add commands as they are typed, don't wait until shell exit 
setopt share_history            # share hist between sessions
setopt bang_hist                # !keyword

# various
setopt auto_cd                  # if command is a path, cd into it
setopt auto_remove_slash        # self explicit
setopt chase_links              # resolve symlinks
setopt correct                  # try to correct spelling of commands
setopt extended_glob            # activate complex pattern globbing
setopt glob_dots                # include dotfiles in globbing
setopt print_exit_value         # print return value if non-zero
unsetopt beep                   # no bell on error
unsetopt bg_nice                # no lower prio for background jobs
unsetopt clobber                # must use >| to truncate existing files
unsetopt hist_beep              # no bell on error in history
unsetopt hup                    # no hup signal at shell exit
unsetopt ignore_eof             # do not exit on end-of-file
unsetopt list_beep              # no bell on ambiguous completion
unsetopt rm_star_silent         # ask for confirmation for `rm *' or `rm path/*'
# setxkbmap -option compose:ralt  # compose-key... shell doesn't seem to like this

# aliases
# ------------------------------------------------------------------------------ 
# general
alias emacs='printf "\xf0\x9f\x98\x90  Dont do that.\n"'                # fun
alias ls='ls -aGh'                                   
alias ll='ls -aGh1'                                   
alias lv='ls -aGhl'                                   
alias p='ps'                                   
alias vi=/usr/local/Cellar/neovim/0.1.4/bin/nvim                        # lazy nvim
alias vim=/usr/local/Cellar/vim/7.4.1910/bin/vim                      # lazy Cellar vim 

# utility scripts 
# apple scripts
alias quit_all='cd; cd .bin/appl/; osascript goto_safari.scpt'
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'

# ssh 
# tamu
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                  # compute server
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                      # linux server
# et al
if [ -f ~/.ssh_aliases ]; 
then
    source ~/.ssh_aliases
fi

# git
alias git_shove='./.bin/shell/git_shove.sh'                              # quick commit
alias git_up_subs='git submodule foreach git pull origin master'        # update submods

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# mysql (testing)
alias mysqladmin='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u root -h localhost -p'
alias mysqluser='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u eric0112 -h localhost -p'

