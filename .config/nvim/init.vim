" general
" ------------------------------------------------------------------------------------------
execute pathogen#infect()|                              " Pathogen package manager

syntax on                                               " enable syntax check, linting, etc
filetype plugin on                                      " enable plugins
filetype plugin indent on                               " enable indent by plugin(s)
let $BASH_ENV = "$HOME/.bashrc"                         " talk to .bashrc

" settings if using regular vim and not nvim
if !has('nvim') 
    set autoindent                                      " auto indents supposedly
    set backspace=eol,start,indent                      " enable backspace
    set hlsearch                                        " highlight search results
    set laststatus=2                                    " always show bottom status bar
    set mouse=a                                         " enable mouse
    set nocompatible                                    " not compatible with vi
    set lazyredraw                                      " redraw lines only when needed
    set smarttab                                        " whitespace/tab stuff 
    set t_Co=256                                            " use 250 terminal colors
    set wildmenu                                        " bash-like tab comp
endif

set autoread                                            " reads external file updates
set clipboard=unnamed                                   " enable clipboard access
set cursorline                                          " vi knows current line 
set expandtab                                           " whitespace/tab stuff 
set history=100                                         " history log 
set ignorecase                                          " ignore casing when searching 
set linebreak                                           " break lines
set nofoldenable                                        " disable line folding
set nolist                                              " list disables linebreak
set number                                              " line numbers
set numberwidth=5                                       " line number column width
set relativenumber                                      " relative line numbers
set ruler                                               " show file data at bottom right
set shiftwidth=4                                        " whitespace/tab stuff
set showcmd                                             " show commands as typed
set smartcase                                           " specify casing in searching 
set splitbelow                                          " always split windows below
set softtabstop=4                                       " whitespace/tab stuff 
set showtabline=2                                       " always show tabline
set tabstop=4                                           " whitespace/tab stuff
set textwidth=90                                        " break line after 90 chars
set undolevels=100                                      " how much undo remembers 
set visualbell                                          " please no beeping
set wildmode=longest,list                               " bash-like tab comp
set wrap                                                " wrap lines
set wrapmargin=0                                        " warp line margin size

autocmd BufNewFile,BufRead *.py set textwidth=79        " if .py break line after x chars
autocmd! BufWritePost * Neomake                         " async run Neomake upon write

" aesthetic 
" ------------------------------------------------------------------------------------------
colorscheme badwolf                                     " rad colorscheme 

" color settings
highlight CursorLine ctermbg=none ctermfg=none
highlight CursorLineNR ctermbg=gray ctermfg=black
highlight LineNr ctermbg=darkgray ctermfg=white
highlight MatchParen cterm=none ctermbg=gray 
highlight Normal ctermbg=none ctermfg=none
highlight SignColumn ctermbg=none
highlight Visual ctermbg=gray ctermfg=none

" functions 
" ------------------------------------------------------------------------------------------
" turn on normal line numbers 
function! NormLines()
    set relativenumber!
endfunc

" keybindings
" ------------------------------------------------------------------------------------------
" regular
nmap ; :|                                               " rebind colon to semicolon
nmap gs <c-w><c-w>|                                     " better window jumping
imap jj <esc>|                                          " rebind escape to jj 

" leader
let mapleader = " "|                                    " bind leader to spacebar

nmap <leader>` :e $MYVIMRC<cr>|                         " quick access to this file 
nmap <leader>1 :w<cr> :<c-p><cr>|                       " write then redo previous command 
nmap <leader>2 :!goto_safari<cr>|                       " call appl.scpt
map <leader>- <c-b>|                                    " page up
map <leader>= <c-f>|                                    " page down 

nmap <leader>r :call NormLines()<cr>|                   " change line numbers 
nmap <leader>t :tabe<cr>|                               " new tab 
nmap <leader>i 0i<cr><esc>k|                            " insert line
nmap <leader>[ <c-t>|                                   " return from def ctag
nmap <leader>] <c-]>|                                   " goto fun def ctag

nmap <leader>s :split<cr>|                              " new window
nmap <leader>d :e .<cr>|                                " open directory
nmap <leader>l `.|                                      " jump to last edit 

nmap <leader>c :!ctags -R .|                            " make ctags in dir 

" et al 
" ------------------------------------------------------------------------------------------
" Lightline settings
let g:lightline = {
            \ 'colorscheme': 'wombat',
            \ 'active' : {
            \ 'left': [ [ 'mode', 'paste' ],
            \           [ 'readonly', 'filename', 'modified' ] ],
            \ 'right': [ [ 'lineinfo' ],
            \            [ 'percent' ],
            \            [ 'fileformat', 'fileencoding', 'filetype' ] ] ,
            \ },
            \ 'tabline': {
            \ 'left': [ [ 'tabs' ] ],
            \ 'right': [ [ ] ] 
            \ },
            \ }
