" Filename: .vimrc
" Note:     Trusty settings.
"

"
" Plug-Ins
"

call plug#begin('~/.config/nvim/plugs')
Plug 'neovimhaskell/haskell-vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tomasiser/vim-code-dark'
call plug#end()

"
" General
"

syntax on
filetype on
filetype plugin on
filetype plugin indent on

set autochdir
set autoindent
set autoread
set background=dark
set backspace=eol,start,indent
set clipboard=unnamed
set colorcolumn=72
set confirm
set expandtab
set foldcolumn=1
set formatoptions+=t
set hidden
set history=100
set hlsearch
set ignorecase
set laststatus=2
set list
set listchars=tab:>\ ,nbsp:_,trail:.
set mouse=a
set nocompatible
set nofoldenable
set noswapfile
set ruler
set shell=bash
set shiftwidth=4
set shortmess+=I
set showcmd
set smartcase
set smarttab
set softtabstop=4
set spelllang=en_us
set splitbelow
set tabstop=4
set textwidth=0
set undolevels=500
set visualbell
set wildmenu
set wildmode=list:longest,list

"
" Hooks
"

" Haskell
aug haskell
    au!
    au FileType haskell setlocal expandtab shiftwidth=4 softtabstop=4
aug END

" Git
au FileType gitcommit setlocal textwidth=72 spell

" Plain text
au FileType text setlocal textwidth=72 spell

" VimL
au FileType vim setlocal textwidth=72 formatoptions+=t

" Writing
"
" Remove all trailing whitespace upon write
au BufWritePre * %s/\s\+$//e

"
" Keybindings
"

" Escape from the homerow.
inoremap hh <esc>

" Swap ; and :
nnoremap ; :
nnoremap : ;

" Swap ; and :
vnoremap ; :
vnoremap : ;

let mapleader = "\<space>"

" Replay last keyboard macro.
nnoremap <leader>2 @@

" Jump to last edit and center.
nnoremap <leader>l `.zz

" Faster window jumping.
nnoremap <leader>o <c-w><c-w>

" - Split window horizontally.
" - Split window vertically.
" - Close other windows except for the current one.
nnoremap <leader>wh :split<cr>
nnoremap <leader>wv :vsplit<cr>
nnoremap <leader>ww :only<cr>

"
" Colors, UI, Etc
"

" That bar down there.
set statusline=                     " Reset
set statusline+=%.20f               " Full path truncated to 20 characters
set statusline+=\ %m                " Indicate if file has beed modified
set statusline+=\ \|\ FileType:\ %y " Filetype
set statusline+=\ \|\ L:\ %3l\/%-L  " Line/total lines
set statusline+=\ \|\ C:\ %3c       " Column number
set statusline+=\ \|\ %p%%          " Percent file

" Theme
colorscheme codedark

"
" GUI
"

if has("gui_running")
    set guifont=Menlo:h14
endif
