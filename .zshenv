# ~/.zshenv
# ------------------------------------------------------------------------------

# Variables
# ------------------------------------------------------------------------------
export FZF_DEFAULT_OPTS='
--color fg:252,bg:-1,hl:67,fg+:252,bg+:235,hl+:81
--color info:144,prompt:161,spinner:135,pointer:135,marker:118'                 # fzf colors (monokai)
export FZF_DEFAULT_COMMAND='ag --hidden --silent --ignore .git -f -g ""' # fzf search 

# Aliases
# ------------------------------------------------------------------------------
alias em='emacsclient -tty'                                                     # Plug into terminal Emacs.
alias emacs='emacsclient'                                                       # Plug into GUI Emacs.
alias ls='ls -aGh'
alias ll='ls -aGh1'
alias lv='ls -aGhl'
alias vi='/usr/local/opt/neovim/bin/nvim'                                       # Neovim.
alias vim='/usr/local/opt/neovim/bin/nvim'                                      # And Neovim.

# Apple Scripts
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'

# Utility Scripts
alias quitter='./.bin/sh/quitter.sh'

# ssh
# TAMU
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                          # Compute Server.
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                              # Linux Server.
# Et al
if [ -f ~/.ssh_aliases ];
then
    source ~/.ssh_aliases
fi

# Git
alias git_shove_config='./.bin/sh/git_shove_config.sh'                          # Quick commit.
alias git_up_subs='git submodule foreach git pull origin master'                # Update submodules.

# Shell Integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# MySQL (testing)
alias mysqladmin='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u root -h localhost -p'
alias mysqluser='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u eric0112 -h localhost -p'
