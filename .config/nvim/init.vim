" general
" ------------------------------------------------------------------------------ 
execute pathogen#infect()|                              " Pathogen package manager

syntax on                                               " enable syntax linting
filetype plugin on                                      " enable plugins
filetype plugin indent on                               " enable indent by plugin(s)

let $BASH_ENV =  "$HOME/.bashrc"                        " talk to .bashrc
let python_highlight_all = 1                            " better python syntax hilighting
let g:netrw_liststyle=3                                 " netrw listing with vertical lines

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
    set t_Co=256                                        " use 250 terminal colors
    set wildmenu                                        " bash-like tab comp
endif

set autoread                                            " reads external file updates
set clipboard=unnamed                                   " enable clipboard access
set cursorline                                          " vi knows current line 
set expandtab                                           " whitespace/tab stuff 
set foldcolumn=1                                        " single whitespace indentation on left margin
set history=100                                         " history log 
set ignorecase                                          " ignore casing when searching 
set nofoldenable                                        " disable line folding
set numberwidth=5                                       " line number column width
set rtp+=/usr/local/opt/fzf                             " fzf plugin path
set ruler                                               " show line+column at bottom right
set guitablabel=%t
set shell=zsh\ -l
set shiftwidth=4                                        " whitespace/tab stuff
set showcmd                                             " show commands as typed
set smartcase                                           " specify casing in searching 
set splitbelow                                          " always split windows below
set softtabstop=4                                       " whitespace/tab stuff 
set showtabline=2                                       " always show tabline
set tabstop=4                                           " whitespace/tab stuff
set textwidth=79                                        " break line after 90 chars
set undolevels=500                                      " how much undo remembers 
set visualbell                                          " no beeping
set wildmode=list:longest,list                          " bash-like tab comp

" automatic commands
au BufNewFile,BufRead * set formatoptions+=t            " set textwidth for code, not comments
au BufNewFile,BufRead .zshrc set textwidth=159          " double textwidth for zshrc 
au! BufWritePost * Neomake                              " async run Neomake upon write

" aesthetic 
" ------------------------------------------------------------------------------ 
colorscheme badwolf                                     " rad colorscheme 

" highlight color settings
hi CursorLine ctermbg=gray ctermfg=black
hi CursorLineNR ctermbg=gray ctermfg=black
hi FoldColumn ctermbg=none
hi MatchParen ctermbg=red ctermfg=none
hi Normal ctermbg=none ctermfg=none
hi SignColumn ctermbg=none
hi StatusLine ctermbg=none ctermfg=gray
hi Visual ctermbg=gray ctermfg=black

" functions 
" ------------------------------------------------------------------------------ 
" none atm 

" keybindings
" ------------------------------------------------------------------------------ 
" regular
imap jj <esc>|                                          " rebind escape to jj 
nmap ; :|                                               " rebind colon to semicolon
nmap gs <c-w><c-w>|                                     " better window jumping

" leader
let mapleader = "\<space>"|                             " bind leader to spacebar

" global 
map <leader>- <c-b>|                                    " page up
map <leader>= <c-f>|                                    " page down

map <leader>j 10j<cr>|                                  " jump 10 lines down
map <leader>k 10k<cr>|                                  " jump 10 lines up

" normal 
nmap <leader>` :e $MYVIMRC<cr>|                         " quick access to this file
nmap <leader>0 zz                                       " recenter window
nmap <leader>2 :Explore<cr>|                            " open file tree
nmap <leader>@ :Explore scp://<cr>|                     " open remote file tree 
nmap <leader>3 :!ctags -R .|                            " make ctags in dir
nmap <leader>9 :e $CLIS<cr>|                            " make ctags in dir

nmap <leader>Q :qall<cr>|                               " quit if everything is saved
nmap <leader>w :!goto_safari<cr>|                       " call appl.scpt
nmap <leader>e :w<cr> :<c-p><cr>|                       " write then redo prev command
nmap <leader>t :tabnew<cr>:Explore .<cr>|               " new tab
nmap <leader>i 0i<cr><esc>k|                            " insert line
nmap <leader>p :Files /Users/eric0112/<cr>|             " fzf by file
nmap <leader>P :Files /|                                " fzf by file
nmap <leader>[ <c-t>|                                   " return from def ctag
nmap <leader>] <c-]>|                                   " goto function def ctag

nmap <leader>s :split<cr>|                              " new window
nmap <leader>f /|                                       " forward search
nmap <leader>F :noh<cr>|                                " undo serach highlighting 
nmap <leader>l `.|                                      " jump to last edit 

nmap <leader>b :buffers <cr>|                           " see buffers
nmap <leader>B :bd |                                    " see buffers

" plugins, et al 
" ------------------------------------------------------------------------------ 
" Multiple Cursors bindings
let g:multi_cursor_quit_key='q'

" fzf settings
let g:fzf_layout = { 'down': '~25%' }

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
