# ~/.zshenv
# ------------------------------------------------------------------------------

# Variables
# ------------------------------------------------------------------------------
# fzf colors
export FZF_DEFAULT_OPTS='
--color fg:252,bg:-1,hl:144,fg+:252,bg+:-1,hl+:228
--color info:144,prompt:114,spinner:-1,pointer:114,marker:193'

# fzf search
export FZF_DEFAULT_COMMAND='ag --hidden --silent --ignore .git -f -g ""'

# Aliases
# ------------------------------------------------------------------------------
# General
alias ctags='/usr/local/opt/ctags/bin/ctags'                                    # ctags.
alias ff='fzf'
alias grep='/usr/local/opt/grep/bin/grep --color'                               # GNU grep.
alias ls='ls -aGh'
alias lsg='ls | /usr/local/opt/grep/bin/grep --color'                           # ls | grep.
alias ll='ls -aGh1'
alias lv='ls -aGhl'
alias lvg='lv | /usr/local/opt/grep/bin/grep --color'                           # lv | grep.
alias python='python3'                                                          # python3.
alias vi='/usr/local/opt/neovim/bin/nvim'                                       # Neovim.
alias vim='/usr/local/opt/neovim/bin/nvim'                                      # And Neovim.

# Git
alias git_shove_config='./.bin/sh/git_shove_config.sh'                          # Quick commit.
alias git_up_subs='git submodule foreach git pull origin master'                # Update submodules.

# Paths
alias cellar='/usr/local/Cellar/'                                               # Homebrew CLI binaries.
# Class paths (tmp).
alias cs206='~/Desktop/strucprog_206/'
alias datasci='~/Desktop/datasci_489/'
alias hci='~/Desktop/hci_436/project/'
alias gamedev='~/Desktop/gamedev_443/'
alias parallel='~/Desktop/parallel_435/'
alias seminar='~/Desktop/seminar_481/'

# Scripts
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'
alias quitter='./.bin/sh/quitter.sh'

# Sql
alias mysqladmin='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u root -h localhost -p'
alias mysqluser='/usr/local/Cellar/mysql/5.7.13/bin/mysql -u eric0112 -h localhost -p'

# Shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# ssh
# TAMU
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                          # Compute Server.
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                              # Linux Server.
# Et al
if [ -f ~/.ssh_aliases ];
then
    source ~/.ssh_aliases
fi
