" Filename: .vimrc
" Note:     Editor of the Beast config. Plugin free. Should work with most
"           modern Vims.

"----------
" Defaults
"----------

syntax off
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
set shiftwidth=2
set shortmess+=I
set showcmd
set smartcase
set smarttab
set softtabstop=2
set spelllang=en_us
set splitbelow
set t_Co=256
set tabstop=2
set termguicolors
set textwidth=80
set timeoutlen=200
set undolevels=500
set visualbell
set wildmenu
set wildmode=list:longest,list

"------------
" Hooks, Etc
"------------

" Etc
au BufWritePre * %s/\s\+$//e           " remove all trailing whitespace upon write
au BufWritePre * %s/\n\{3,}/\r\r/e     " condense all blank lines into one
au BufWritePre * %s:\($\n\s*\)\+\%$::e " no blank lines at EOF, please

" Plain Text
aug plainText
  au!
  au FileType gitcommit setlocal
        \ spell
        \ textwidth=72
  au FileType markdown,tex,text setlocal spell
aug END

" VimL
au FileType vim setlocal formatoptions+=t

"-------------
" Keybindings
"-------------

" Defaults
" escape escaping
inoremap hh <esc>
" swap ; and :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" navigate splits
nnoremap <silent> <up>    <c-w>k
nnoremap <silent> <down>  <c-w>j
nnoremap <silent> <left>  <c-w>h
nnoremap <silent> <right> <c-w>l
" Leader Bindings
let mapleader = "\<space>"
" visit previous buffer
nnoremap <leader>r :b#<cr>
" toggle spellchecker
nnoremap <leader>s :setlocal spell!<cr>
" replay keyboard macro at q over selected region
vnoremap <leader>. :norm@q<cr>

"-----------------
" Colors, UI, Etc
"-----------------

set statusline=                        " reset
set statusline+=\ %.40f                " relative filepath (40 char max width)
set statusline+=\%m                    " if modified
set statusline+=\ %y                   " filetype
set statusline+=%=                     " spacer
set statusline+=\%l:%c                 " line:column numbers
set statusline+=\ \ \ \ \ \ \ \ \ %P\  " buffer percentage

highlight EndOfBuffer ctermfg=none " don't color the tiles!
