" Filename: dotvim.vim
" Note:     Lean (mean?) NeoVim config. Hopefully we ditch VimL soon.

call plug#begin('~/.config/vim/plugs')
Plug 'arcticicestudio/nord-vim'
Plug 'lervag/vimtex'
Plug 'neoclide/coc.nvim'
Plug 'neovimhaskell/haskell-vim'
Plug 'plasticboy/vim-markdown'
Plug 'tpope/vim-commentary'
call plug#end()

syntax on
filetype on
filetype plugin on
filetype plugin indent on

" General
set autochdir
set confirm
set hlsearch
set ignorecase
set incsearch
set list
set listchars=tab:»\ ,trail:. " yuck
set mouse=a
set nofoldenable
set noswapfile
set spelllang=en_us
set textwidth=80
set timeoutlen=200

" Indentation
set autoindent
set expandtab
set shiftwidth=2
set tabstop=2

" Keybindings
inoremap hh <esc>
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Status Bar
set laststatus=2
set ruler
set noshowcmd
set statusline=                        " reset
set statusline+=\ %.40f                " relative filepath (40 char max width)
set statusline+=\%m                    " if modified
set statusline+=\ %y                   " filetype
set statusline+=%=                     " spacer
set statusline+=\%l:%c                 " line:column numbers
set statusline+=\ \ \ \ \ \ \ \ \ %P\  " buffer percentage

highlight EndOfBuffer ctermfg=none " Don't color the tiles!

" GUI
set guicursor= " Disable; breaks blinking on Mac Terminal.

" Hooks
" Java
" turn on these defaults
let java_highlight_functions = 1
let java_highlight_all = 1
highlight li javaScopeDecl Statement
highlight li javaType Type
highlight li javaDocTags PreProc
autocmd FileType java setlocal textwidth=120

augroup plainText
  autocmd!
  autocmd FileType gitcommit setlocal
        \ spell
        \ textwidth=72
  autocmd FileType markdown,tex,text setlocal spell
augroup END

" PlugIns
" CoC
set nobackup
set nowritebackup
set updatetime=300
" 0. Use <tab> for trigger completion and navigate to the next complete item with.
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction
" 1. Then bind it
inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

" Nord Colors
colorscheme nord
