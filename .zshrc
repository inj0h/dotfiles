# general
# ------------------------------------------------------------------------------------------
# init 
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
PROMPT="%{$fg[$color]%}%n%{$reset_color%}@%U%{$fg[yellow]%}%m%{$reset_color%}%u %B%~%b
%{$fg[$color]%}%#%{$reset_color%} %{$fg[green]%}$ %{$reset_color%}"
RPROMPT='${vcs_info_msg_0_}'

#export LSCOLORS=GxFxDxBxCxegedabagacad                                  
# order of lscolors from left to right..
# directory = G
# symlink = F
# socket = D
# pipe = B
# executable = C
# B = red 
# C = green
# D = brown
# F = magenta
# G = cyan

# key bindings
# Lookup in /etc/termcap or /etc/terminfo else, you can get the right keycode
# by typing ^v and then type the key or key combination you want to use.
# "man zshzle" for the list of available actions

# aliases
# ------------------------------------------------------------------------------------------
# shopt -s expand_aliases                                                 # vim reads aliases

# general
alias emacs='printf "\xf0\x9f\x98\x90  Dont do that.\n"'                # fun
alias ls='ls -aGh'                                   
alias vi=/usr/local/Cellar/neovim/0.1.4/bin/nvim                        # lazy nvim
alias vim=/usr/local/Cellar/vim/7.4.1847_1/bin/vim                        # lazy Cellar vim 

# apple scripts 
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'

# utility scripts 

# ssh 
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                  # compute server
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                      # linux server

# git
alias git_shove='./.bin/bash/git_shove.sh'                              # quick commit
alias git_up_subs='git submodule foreach git pull origin master'        # update submods

# init 
# ------------------------------------------------------------------------------------------
#export PS1="\u @ \h \w\n$ "                         # prompt
#export TERM='xterm-256color'                        # terminal type
printf "Welcome $USER! \xf0\x9f\x98\x84  \n"        # welcome message 
