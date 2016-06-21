" general
" ------------------------------------------------------------------------------ 
execute pathogen#infect()|                              " Pathogen package manager

syntax on                                               " enable syntax linting
filetype plugin on                                      " enable plugins
filetype plugin indent on                               " enable indent by plugin(s)

let python_highlight_all = 1                            " better python syntax highlighting
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
" ------------------------------------------------------------------------------ 
"not sure if I need these atm
"set listchars=tab:>-,nbsp:_,trail:.                    " make tabs + trailing spaces visible
"set list                                               " ^
" ------------------------------------------------------------------------------ 
set nofoldenable                                        " disable line folding
set numberwidth=5                                       " line number column width
set rtp+=/usr/local/opt/fzf                             " fzf plugin path
set ruler                                               " show line+column at bottom right
set shell=zsh\ -l                                       " shell = zsh
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
set wildmode=list:longest,list                          " tab completion

" automatic commands
au BufNewFile,BufRead * set formatoptions+=t            " set textwidth for code, not comments
au BufNewFile,BufRead .txt set spell spelllang=en_us    " spell checking for .txt
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
hi SpellBad ctermbg=Blue ctermfg=black  
hi SpellCap ctermbg=none
hi SpellLocal ctermbg=Blue ctermfg=black
hi SpellRare ctermbg=Blue ctermfg=black
hi StatusLine ctermbg=none ctermfg=gray
hi Visual ctermbg=gray ctermfg=black

" functions 
" ------------------------------------------------------------------------------ 
" turn on linter for spelling (en_us)
function! LintSpell()
    set spell!
endfunc

" keybindings
" ------------------------------------------------------------------------------ 
" regular
inoremap jj <esc>|                                      " rebind escape to jj 

nmap ; :|                                               " rebind colon to semicolon
nmap gs <c-w><c-w>|                                     " better window jumping

" leader
let mapleader = "\<space>"|                             " bind leader to spacebar

" global 
noremap <leader>- <c-b>|                                " page up
noremap <leader>= <c-f>|                                " page down

noremap <leader>j 10j<cr>|                              " jump 10 lines down
noremap <leader>k 10k<cr>|                              " jump 10 lines up

" normal 
nmap <leader>f /|                                       " find regex, no nnoremap b/c SearchComplete

nnoremap <leader>` :e $MYVIMRC<cr>|                     " quick access to this file
nnoremap <leader>0 zz                                   " recenter window
nnoremap <leader>1 :e . <cr>|                           " open file tree
nnoremap <leader>! :e scp://|                           " open remote file tree 
nnoremap <leader>2 @@|                                  " replay key macro q
nnoremap <leader>@ @q|                                  " play key macro q
nnoremap <leader>3 :!ctags -R .|                        " make ctags in dir
nnoremap <leader>4 :call LintSpell()<cr>|               " call function
nnoremap <leader>9 :e $CLIS<cr>|                        " open file

nnoremap <leader>Q :qall<cr>|                           " quit if everything is saved
nnoremap <leader>w :!goto_safari<cr>|                   " call appl.scpt
nnoremap <leader>r :w<cr> :<c-p><cr>|                   " write then redo prev command
nnoremap <leader>t :tabnew<cr>|                         " new tab
nnoremap <leader>T :tabnew<cr>:e .<cr>|                 " new tab + file tree
nnoremap <leader>i 0i<cr><esc>k|                        " insert line
nnoremap <leader>p :Files /Users/<cr>|                  " fzf by file
nnoremap <leader>P :Files /|                            " fzf by file from /
nnoremap <leader>[ <c-t>|                               " return from def ctag
nnoremap <leader>] <c-]>|                               " goto function def ctag

nnoremap <leader>s :split<cr>|                          " split new window below
nnoremap <leader>F :noh<cr>|                            " undo find highlighting 
nnoremap <leader>l `.|                                  " jump to last edit 

nnoremap <leader>b :buffers <cr>|                       " see buffers
nnoremap <leader>B :bd |                                " delete buffer(s)

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
            \ 'left': [ ['paste' ],
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
