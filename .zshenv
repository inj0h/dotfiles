# ~/.zshenv
# ------------------------------------------------------------------------------

# variables
# ------------------------------------------------------------------------------
export FZF_DEFAULT_OPTS='
--color fg:252,bg:233,hl:67,fg+:252,bg+:235,hl+:81
--color info:144,prompt:161,spinner:135,pointer:135,marker:118'                 # fzf colors (monokai)
export FZF_DEFAULT_COMMAND='ag --hidden --silent --ignore .git -f -g ""'        # fzf search

# aliases
# ------------------------------------------------------------------------------
# easy access
alias em='emacsclient -nw'                                                      # plug into gui emacs
alias emacs='emacsclient'                                                       # plug into gui emacs
alias ls='ls -aGh'
alias ll='ls -aGh1'
alias lv='ls -aGhl'
alias p='ps'
alias vi=/usr/local/opt/neovim/bin/nvim                                         # lazy nvim
alias vim=/usr/local/opt/vim/bin/vim                                            # lazy Cellar vim

# utility scripts
# apple scripts
alias quitter='./.bin/sh/quitter.sh'
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'

# ssh
# tamu
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                          # compute server
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                              # linux server
# et al
if [ -f ~/.ssh_aliases ];
then
    source ~/.ssh_aliases
fi

# git
alias git_shove_config='./.bin/sh/git_shove_config.sh'                          # quick commit
alias git_up_subs='git submodule foreach git pull origin master'                # update submods

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# mysql (testing)
alias mysqladmin='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u root -h localhost -p'
alias mysqluser='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u eric0112 -h localhost -p'
