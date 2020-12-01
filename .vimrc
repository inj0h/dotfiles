" Filename: .vimrc
" Note:     No BS editor config. Requires Vim 8 or l8r compiled with
"           termguicolors. Otherwise, just change the colorscheme.

" TODO:
" - Configure indentation, text-width, etc for...
"     - C/C++
"     - JSON
"     - Markdown
"     - Swift
"     - TeX
"     - TypeScript
"     - YAML

"---------
" Plugins
"---------

call plug#begin('~/.config/vim/plugs')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'georgewitteman/vim-fish'
Plug 'junegunn/vim-easy-align'
Plug 'neoclide/coc.nvim'
Plug 'neovimhaskell/haskell-vim'
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

" Git
au FileType gitcommit setlocal
      \ spell
      \ textwidth=72

" Haskell
aug haskell
  au!
  au FileType haskell setlocal formatoptions+=t
aug END

" Java
" turn on these defaults
let java_highlight_functions = 1
let java_highlight_all = 1
hi li javaScopeDecl Statement
hi li javaType Type
hi li javaDocTags PreProc

" Plain Text
aug plainText
  au!
  au BufEnter *.md,*.tex,*.txt silent loadview
  au BufLeave *.md,*.tex,*.txt mkview
  au FileType text setlocal
        \ spell
        \ foldmethod=marker
        \ foldmarker=[,]
        \ foldenable
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

colorscheme bluewery
" technically a Plugin
" add the following line in the colorscheme file to color FoldColumn..
" call bluewery#hi('FoldColumn',       '', '',          s:b_black)

"-----------------
" Plugin Settings
"-----------------

" CoC
" recommendations
set nobackup
set nowritebackup
set updatetime=300
" use <tab> for trigger completion and navigate to the next complete item with
" this bit of VimL
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction
" then bind it
inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

" CtrlP
if executable('rg')
  let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob ""'
endif
nnoremap <leader>o :CtrlP<cr>
nnoremap <leader>e :CtrlPBuffer<cr>

" Vim Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
