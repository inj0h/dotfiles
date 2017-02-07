# ~/.zshenv
# ------------------------------------------------------------------------------


# variables
# ------------------------------------------------------------------------------
# fzf colors
export FZF_DEFAULT_OPTS='
--color fg:252,bg:-1,hl:144,fg+:252,bg+:-1,hl+:228
--color info:144,prompt:114,spinner:-1,pointer:114,marker:193'

# fzf search
export FZF_DEFAULT_COMMAND='ag --hidden --silent --ignore .git -f -g ""'


# aliases
# ------------------------------------------------------------------------------
# source my aliases
if [ -f ~/.my_aliases ];
then
    source ~/.my_aliases
fi

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# ssh
if [ -f ~/.ssh_aliases ];
then
    source ~/.ssh_aliases
fi
