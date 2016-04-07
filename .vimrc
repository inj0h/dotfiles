" erikoelrojo's .vimrc
" -------------------------------------------------------------------------------- 








" general
" -------------------------------------------------------------------------------- 
" Pathogen package manager
execute pathogen#infect()                                       

syntax on                                           " enable syntax check, linting, etc
filetype plugin indent on                           " enable indent by plugin(s)
filetype plugin on                                  " enable plugins




set nocompatible                                    " ?

set autoindent                                      " auto indents supposedly
set autoread                                        " ?
set backspace=eol,start,indent                      " enable backspace
set clipboard=unnamed                               " enable clipboard access
set cursorline                                      " enable current line recognition 
set expandtab                                       " whitespace/tab stuff 
set ignorecase                                      " ignore casing when searching 
set history=100                                     " history log 
set hlsearch                                        " highlight search results
set laststatus=2                                    " always enable Lightline plugin
set lazyredraw                                      " redraw lines only when needed
set linebreak                                       " break lines
set mouse=a                                         " enable mouse
set nofoldenable                                    " disable line folding
set nolist                                          " list disables linebreak
set number                                          " line numbers
set relativenumber                                  " relative line numbers
set ruler                                           " ? 
set shiftwidth=4                                    " whitespace/tab stuff
set smartcase                                       " specify casing in searching 
set smarttab                                        " whitespace/tab stuff 
set splitbelow                                      " always split windows below
set softtabstop=4                                   " whitespace/tab stuff 
set t_Co=256                                        " use 250 terminal colors
set tabstop=4                                       " whitespace/tab stuff
set textwidth=0                                     " text width (duh)
set undolevels=50                                   " how much undo remembers 
set visualbell                                      " please no beeping
set wrap                                            " wrap lines
set wrapmargin=0                                    " warp line margin size




" aesthetic 
" -------------------------------------------------------------------------------- 
colorscheme badwolf                                 " rad colorscheme 

" color settings
highlight CursorLine ctermbg=none ctermfg=none
highlight CursorLineNR ctermbg=gray ctermfg=black
highlight LineNr ctermbg=none 
highlight MatchParen cterm=none ctermbg=gray 
highlight Normal ctermbg=none ctermfg=none
highlight Visual ctermbg=gray ctermfg=none








" keybindings
" -------------------------------------------------------------------------------- 
" regular
" ----------------------------------------
" rebind colon to semicolon
nmap ; : 
" better window jumping
nmap gs <c-w><c-w> 
" rebind escape to jj 
imap jj <esc>




" leader
" ----------------------------------------
" leader bound to spacebar
let mapleader = " "


" quick access to this file 
nmap <leader>` :e $MYVIMRC<cr>
" page up
nmap <leader>- <c-b>
" page down 
nmap <leader>= <c-f>


" new tab 
nmap <leader>t :tabe<cr> 
" insert line
nmap <leader>i 0i<cr><esc>k


" new window
nmap <leader>s :split<cr>
" open directory
nmap <leader>d :e .<cr>
" jump to last edit 
nmap <leader>l `. 








" et al 
" -------------------------------------------------------------------------------- 
" Lightline settings
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }
