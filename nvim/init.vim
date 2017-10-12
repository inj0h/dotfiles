" General
" ------------------------------------------------------------------------------
execute pathogen#infect()|                              " Pathogen package manager

syntax on                                               " Enable syntax linting
filetype on                                             " Enable filetype variable
filetype plugin on                                      " Enable plugins
filetype plugin indent on                               " Enable indent by plugin(s)

" Turn these on if using regular vim and not nvim
if !has('nvim')
    set autoindent                                      " Auto-indentation
    set backspace=eol,start,indent                      " Enable backspace
    set hlsearch                                        " Highlight search results
    set guioptions-=l                                   " Remove left GUI scrollbar
    set guioptions-=L                                   " Remove left GUI split scrollbar
    set guioptions-=r                                   " Remove right GUI scrollbar
    set guioptions-=R                                   " Remove right GUI split scrollbar
    set laststatus=2                                    " Always show bottom status bar
    set nocompatible                                    " Disable vi compatibility
    set smarttab                                        " Whitespace/tab stuff
    set t_Co=256                                        " Use 250 terminal colors
    set wildmenu                                        " Tab completion
endif

" Enable truecolors only if term supports it!
if $COLORTERM == 'truecolor'
    set termguicolors
endif

set autoread                                            " Reads external file updates
set background=dark                                     " Goth background
set clipboard=unnamed                                   " Enable clipboard access
set confirm                                             " Manage buffer state before exiting
set cursorline                                          " Show current line
set expandtab                                           " Set tabs = spaces
set foldcolumn=0                                        " Whitespace indentation on left margin
set formatoptions+=t                                    " Set textwidth for code but not comments
set guicursor=n:blinkon1                                " Blink cursor
set hidden                                              " Leave modded buffer
set history=100                                         " History log
set ignorecase                                          " Ignore casing when searching
set list                                                " ^
set listchars=tab:>-,nbsp:_,trail:.                     " Make tabs + trailing spaces visible
set mouse=a                                             " Enable mouse
set nofoldenable                                        " Disable line folding
set number                                              " Line numbers
set relativenumber                                      " Relative line numbers (if both enabled use both)
set rtp+=/usr/local/opt/fzf                             " Set FZF plugin path
set ruler                                               " Show line+column at bottom right
set shell=zsh                                           " Shell = zsh
set shiftwidth=4                                        " Width of indent in spaces
set shortmess+=I                                        " Disable splash screen
set showcmd                                             " Show commands as typed
set smartcase                                           " Specify casing in searching
set softtabstop=4                                       " Whitespace/tab stuff
set spelllang=en_us                                     " Parse English
set splitbelow                                          " Always split windows below
set tabstop=8                                           " Width of tab char in spaces
set textwidth=0                                         " Textwidth = window width
set undolevels=500                                      " Extent of undo remembers
set visualbell                                          " No beeping
set wildmode=list:longest,list                          " Tab completion

" Automatic Commands
" ------------------------------------------------------------------------------
" By Plugin

" Neomake
" Async run Neomake upon write
"autocmd! BufEnter,BufReadPost,BufWritePost * Neomake
autocmd! BufWritePost * Neomake

" By FileType, et al

" General
" Remove all trailing whitespace upon write
autocmd BufWritePre * %s/\s\+$//e

" Algol lang
" Wrap curly braces and insert b/w
autocmd FileType c,c++,swift,javascript inoremap {<tab> {<cr>}<esc>O

" Func lang
augroup rainbow_lisp
    autocmd!
    autocmd FileType lisp,clojure,scheme,haskell RainbowParentheses
augroup END

" pep8
autocmd FileType python setlocal textwidth=80 tabstop=4 fileformat=unix

" txt
autocmd FileType text setlocal textwidth=80 spell
" autocmd BufWrite *.txt :normal ggVGgq`.zz

" VimL
autocmd FileType vim setlocal textwidth=80 formatoptions+=t

" Aesthetic
" ------------------------------------------------------------------------------
if $COLORTERM == 'truecolor'
    colorscheme cueva
else
    colorscheme desert
endif

" Functions
" ------------------------------------------------------------------------------
" Toggle line numbers
function ToggleLines()
    if(&relativenumber == 1)
        set norelativenumber
    else
        set relativenumber
    endif
endfunc

" Enable spell linter (en_us)
function LintSpell()
    set spell!
endfunc

" Keybindings
" ------------------------------------------------------------------------------
" Regular
inoremap jj <esc>|                                      " Rebind escape

nnoremap ; :|                                           " Rebind colon
nnoremap : ;|                                           " Rebind semicolon

nmap <c-a> 0|                                           " Rebind beginning of line
nmap <c-e> $|                                           " Rebind end of line
vmap <c-a> 0|                                           " Rebind beginning of line (visual)
vmap <c-e> $|                                           " Rebind end of line (visual)

" BUG: These two bindings slow down 'd' in both normal and visual modes.
"nmap d<c-e> d$|                                         " Delete to end of line
"vmap d<c-e> d$|                                         " Delete to end of line (visual)

nmap <c-f> <c-f>M|
nmap <c-b> <c-b>M|
vmap <c-f> <c-f>M|
vmap <c-b> <c-b>M|

" Leader
let mapleader = "\<space>"|                             " Bind leader to spacebar

" Leader General
noremap <leader>- <c-b><bar>M|                          " Page up
noremap <leader>= <c-f><bar>M|                          " Page down

" Leader Normal
nnoremap <leader>` :e $MYVIMRC<cr>|                     " Quick access to this file
nnoremap <leader>2 @@|                                  " Replay key macro q
nnoremap <leader>3 :!ctags -R .|                        " Make ctags in dir
nnoremap <leader>4 :call LintSpell()<cr>|               " Call function

nnoremap <leader>Q :qall<cr>|                           " Quit if everything is saved
nnoremap <leader>w :Buffers <cr>|                       " FZF to list buffer(s)
nnoremap <leader>y 0v$hy<cr>|                           " Yank a line without \n
nnoremap <leader>i 0i<cr><esc>k|                        " Insert line
nnoremap <leader>o :Files <cr>|                         " FZF to search/open child file(s)
nnoremap <leader>O :Files |                             " FZF to search/open w/ given path
nnoremap <leader>[ <c-t>|                               " Return from def ctag
nnoremap <leader>] <c-]>|                               " Goto function def ctag

nnoremap <leader>f :Lines <cr>|                         " FZF to search thru open buffers
nnoremap <leader>s <c-w><c-w>|                          " Faster window jumping
nnoremap <leader>S :split<cr>|                          " Split new window below
nnoremap <leader>l `.zz|                                " Jump to last edit and center
nnoremap <leader>L :call ToggleLines()<cr>

nnoremap <leader>/ :noh<cr>|                            " Undo find highlighting

" Plugin Settings
" ------------------------------------------------------------------------------
" FZF
let g:fzf_layout = { 'up': '~40%' }

" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'cueva'

" Language Support
" ------------------------------------------------------------------------------
" Haskell
let g:haskell_classic_highlighting = 1

" Python
let python_highlight_all = 1                            " Better python syntax highlighting.
