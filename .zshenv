# ~/.zshenv
# ------------------------------------------------------------------------------ 

# variables 
# ------------------------------------------------------------------------------ 
export CLIS='~/.internship/.clis'

# aliases
# ------------------------------------------------------------------------------ 
# easy access 
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
