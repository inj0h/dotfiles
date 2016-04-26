" general
" -------------------------------------------------------------------------------- 
execute pathogen#infect()|                              " Pathogen package manager

syntax on                                               " enable syntax check, linting, etc
filetype plugin indent on                               " enable indent by plugin(s)
filetype plugin on                                      " enable plugins

let $BASH_ENV = "$HOME/.bashrc"                         " talk to .bashrc

set nocompatible                                        " not compatible with vi
set autoindent                                          " auto indents supposedly
set autoread                                            " ?
set backspace=eol,start,indent                          " enable backspace
set clipboard=unnamed                                   " enable clipboard access
set cursorline                                          " enable current line recognition 
set expandtab                                           " whitespace/tab stuff 
set ignorecase                                          " ignore casing when searching 
set history=100                                         " history log 
set hlsearch                                            " highlight search results
set laststatus=2                                        " always show bottom status bar
set lazyredraw                                          " redraw lines only when needed
set linebreak                                           " break lines
set mouse=a                                             " enable mouse
set nofoldenable                                        " disable line folding
set nolist                                              " list disables linebreak
set number                                              " line numbers
set relativenumber                                      " relative line numbers
set ruler                                               " ? 
set shiftwidth=4                                        " whitespace/tab stuff
set showcmd                                             " show commands as typed
set smartcase                                           " specify casing in searching 
set smarttab                                            " whitespace/tab stuff 
set splitbelow                                          " always split windows below
set softtabstop=4                                       " whitespace/tab stuff 
set showtabline=2                                       " always show tabline
set t_Co=256                                            " use 250 terminal colors
set tabstop=4                                           " whitespace/tab stuff
set textwidth=0                                         " text width (duh)
set undolevels=50                                       " how much undo remembers 
set visualbell                                          " please no beeping
set wildmode=longest,list                               " bash-like tab comp
set wildmenu                                            " ^ ?
set wrap                                                " wrap lines
set wrapmargin=0                                        " warp line margin size




" aesthetic 
" -------------------------------------------------------------------------------- 
colorscheme badwolf                                     " rad colorscheme 

" color settings
highlight CursorLine ctermbg=none ctermfg=none
highlight CursorLineNR ctermbg=gray ctermfg=black
highlight LineNr ctermbg=none 
highlight MatchParen cterm=none ctermbg=gray 
highlight Normal ctermbg=none ctermfg=none
highlight Visual ctermbg=gray ctermfg=none




" functions 
" -------------------------------------------------------------------------------- 
" turn on normal line numbers 
function! NormLines()
    set relativenumber!
endfunc




" keybindings
" -------------------------------------------------------------------------------- 
" regular
nmap ; :|                                               " rebind colon to semicolon
nmap gs <c-w><c-w>|                                     " better window jumping
imap jj <esc>|                                          " rebind escape to jj 

" leader
let mapleader = " "|                                    " bind leader to spacebar

nmap <leader>` :e $MYVIMRC<cr>|                         " quick access to this file 
nmap <leader>1 :!goto_safari<cr>|                       " call appl.scpt
map <leader>- <c-b>|                                    " page up
map <leader>= <c-f>|                                    " page down 

nmap <leader>r :call NormLines()<cr>|                   " change line numbers 
nmap <leader>t :tabe<cr>|                               " new tab 
nmap <leader>i 0i<cr><esc>k|                            " insert line

nmap <leader>s :split<cr>|                              " new window
nmap <leader>d :e .<cr>|                                " open directory
nmap <leader>l `.|                                      " jump to last edit 




" et al 
" -------------------------------------------------------------------------------- 
" Lightline settings
let g:lightline = {
            \ 'colorscheme': 'wombat',
            \ 'active' : {
            \ 'left': [ [ 'mode', 'paste' ],
            \           [ 'readonly', 'filename', 'modified' ] ],
            \ 'right': [ [ 'lineinfo' ],
            \            [ 'fileformat', 'fileencoding', 'filetype' ] ] ,
            \ },
            \ 'tabline': {
            \ 'left': [ [ 'tabs' ] ],
            \ 'right': [ [ ] ] 
            \ },
            \ }
