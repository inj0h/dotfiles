" Filename: .vimrc
" Note:     No BS editor config. Requires Vim 8 or l8r compiled with
"           termguicolors. Otherwise, just change the colorscheme.

" TODO:
" - Configure indentation, text-width, etc for...
"     - C/C++
"     - Haskell
"     - JSON
"     - Java
"     - Swift
"     - TeX
"     - TypeScript
"     - YAML

"----------
" Plug-Ins
"----------

call plug#begin('~/.config/vim/plugs')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/vim-easy-align'
Plug 'neovimhaskell/haskell-vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'relastle/bluewery.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
call plug#end()

"----------
" Defaults
"----------

syntax on
filetype on
filetype plugin on
filetype plugin indent on

set autoindent
set autoread
set background=dark
set backspace=eol,start,indent
set clipboard=unnamed
set confirm
set expandtab
set foldcolumn=1
set formatoptions+=t
set hidden
set history=100
set hlsearch
set incsearch
set ignorecase
set laststatus=2
set list
set listchars=tab:_\ ,trail:.
set mouse=a
set nocompatible
set nofoldenable
set noswapfile
set ruler
set shell=fish
set shiftwidth=4
set shortmess+=I
set showcmd
set smartcase
set smarttab
set softtabstop=4
set spelllang=en_us
set splitbelow
set t_Co=256
set tabstop=4
set termguicolors
set textwidth=80
set timeoutlen=200
set undolevels=500
set visualbell
set wildmenu
set wildmode=list:longest,list

"-------
" Hooks
"-------

" Git
au FileType gitcommit setlocal textwidth=72 spell

" Haskell
aug haskell
    au!
    au FileType haskell setlocal expandtab shiftwidth=4 softtabstop=4
aug END

" Plain Text
au FileType text setlocal spell

" VimL
au FileType vim setlocal formatoptions+=t

" Writing
" Remove all trailing whitespace upon write
au BufWritePre * %s/\s\+$//e

"-------------
" Keybindings
"-------------

" Defaults
" escape escaping
inoremap hh <esc>
" swap ; and :
nnoremap ; :
nnoremap : ;
" swap ; and :
vnoremap ; :
vnoremap : ;

" Leader
let mapleader = "\<space>"

" Buffers and Windows
" jump to last edit and center
nnoremap <leader>l `.zz
" faster window jumping
nnoremap <leader>n <c-w><c-w>
" visit previous buffer
nnoremap <leader>r :bp<cr>
" split window horizontally
nnoremap <leader>wh :split<cr>
" split window vertically
nnoremap <leader>wv :vsplit<cr>
" close other windows except for the current one
nnoremap <leader>ww :only<cr>

" Macros
" replay last keyboard macro
vnoremap <leader>e :norm@
nnoremap <leader>m @@

" Spelling
" toggle spellchecker
nnoremap <leader>s :setlocal spell!<cr>

"-----------------
" Colors, UI, Etc
"-----------------

set statusline=              " reset
set statusline+=\ %f         " full file path
set statusline+=\%m          " if modified
set statusline+=\ %y         " filetype
set statusline+=%=           " spacer
set statusline+=\%l:%c       " line:column numbers
set statusline+=\ \ \ \ %P\  " buffer percentage

" Technically a Plugin.
colorscheme bluewery
" Add the following line in the colorscheme file to color FoldColumn.
" call bluewery#hi('FoldColumn',       '', '',          s:b_black)

"---------
" Plugins
"---------

" CtrlP
if executable('rg')
  let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob ""'
endif
nnoremap <leader>o :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>

" Vim Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
