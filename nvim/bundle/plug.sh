#!/bin/bash
# Install and update Pathogen managed Vim plugins. 
set -e

# Variables.
INSTALL=""
UPDATE=""
PLUGINS=(https://github.com/hdima/python-syntax
         https://github.com/hynek/vim-python-pep8-indent
         https://github.com/itchyny/lightline.vim
         https://github.com/junegunn/fzf.vim
         https://github.com/neomake/neomake
         https://github.com/neovimhaskell/haskell-vim
         https://github.com/octol/vim-cpp-enhanced-highlight
         https://github.com/pangloss/vim-javascript
         https://github.com/terryma/vim-multiple-cursors
         https://github.com/tpope/vim-fugitive
         https://github.com/tpope/vim-surround
         https://github.com/vim-scripts/SearchComplete)

# Parameters.
if [ "$1" == "" ];
then
    echo "Error. Command requires arguments!"
fi

if [ "$1" == "install" ];
then
    echo "Installing plugins."
    for i in "${PLUGINS[@]}"
    do
        git clone $i 
    done
fi

if [ "$1" == "update" ];
then
    echo "Updating plugins."
    for REPO in `ls | grep -v gruvbox | grep -v 'haskell\.vim' | grep -v mywombat.vim | grep -v plug.sh`
    do
        (cd "$REPO"; git pull);
    done
fi
