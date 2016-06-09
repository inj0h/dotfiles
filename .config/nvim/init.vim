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
set foldcolumn=1
set history=100                                         " history log 
set ignorecase                                          " ignore casing when searching 
set nofoldenable                                        " disable line folding
set numberwidth=5                                       " line number column width
set ruler                                               " show file data at bottom right
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
hi LineNr ctermbg=darkgray ctermfg=white
hi MatchParen ctermbg=red ctermfg=none
hi Normal ctermbg=none ctermfg=none
hi SignColumn ctermbg=none
hi Visual ctermbg=lightgray ctermfg=black

" functions 
" ------------------------------------------------------------------------------ 
" turn on and toggle line numbers 
function! OnLines()
    set foldcolumn=0
    set relativenumber!
    if (&number == 0)
        set number
    endif 
endfunc

" turn off line numbers 
function! OffLines()
    set foldcolumn=1
    if (&relativenumber == 1)
        set relativenumber!
    endif
    if (&number == 1)
        set number!
    endif
endfunc

" keybindings
" ------------------------------------------------------------------------------ 
" regular
imap jj <esc>|                                          " rebind escape to jj 
nmap ; :|                                               " rebind colon to semicolon
nmap gs <c-w><c-w>|                                     " better window jumping

" leader
let mapleader = " "|                                    " bind leader to spacebar

" global 
map <leader>- <c-b>|                                    " page up
map <leader>= <c-f>|                                    " page down

map <leader>j 10j<cr>|                                  " far jump down
map <leader>k 10k<cr>|                                  " far jump up

" normal 
nmap <leader>` :e $MYVIMRC<cr>|                         " quick access to this file
nmap <leader>0 zz                                       " recenter window
nmap <leader>1 :Explore<cr>|                            " open netrw file tree
nmap <leader>3 :!ctags -R .|                            " make ctags in dir

nmap <leader>q :qall<cr>|                               " quit if everything is saved
nmap <leader>w :!goto_safari<cr>|                       " call appl.scpt
nmap <leader>e :w<cr> :<c-p><cr>|                       " write then redo prev command
nmap <leader>t :tabnew<cr>:Explore .<cr>|               " new tab
nmap <leader>i 0i<cr><esc>k|                            " insert line
nmap <leader>[ <c-t>|                                   " return from def ctag
nmap <leader>] <c-]>|                                   " goto function def ctag

nmap <leader>s :split<cr>|                              " new window
nmap <leader>f /|                                       " forward search
nmap <leader>F :noh<cr>|                                " undo serach highlighting 
nmap <leader>l `.|                                      " jump to last edit 

nmap <leader>b :buffers <cr>|                           " see buffers
nmap <leader>B :bd |                                    " see buffers
nmap <leader>n :call OnLines()<cr>|                     " call function 
nmap <leader>N :call OffLines()<cr>|                    " call function 

" plugins, et al 
" ------------------------------------------------------------------------------ 
" Multiple Cursors bindings
let g:multi_cursor_quit_key='q'

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
