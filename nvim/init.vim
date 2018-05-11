" filename:         init.vim
" description:
"                   Personal (neo)vim settings.
"

""""""""""
" plugins
""""""""""

call plug#begin('~/.config/nvim/plugs')
Plug 'erikorojo/cueva'
Plug 'hdima/python-syntax'
Plug 'hynek/vim-python-pep8-indent'
Plug 'junegunn/fzf.vim', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'neomake/neomake'
Plug 'neovimhaskell/haskell-vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'pangloss/vim-javascript'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vividchalk'
Plug 'vim-airline/vim-airline'

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
endif
let g:deoplete#enable_at_startup = 1

call plug#end()

""""""""""
" general
""""""""""

syntax on                               " enable syntax linting
filetype on                             " enable filetype variable
filetype plugin on                      " enable plugins
filetype plugin indent on               " enable indent by plugin(s)

" turn these on if using regular vim and not nvim
if !has('nvim')
    set autoindent                      " auto-indentation
    set backspace=eol,start,indent      " enable backspace
    set hlsearch                        " highlight search results
    set guioptions-=l                   " remove left GUI scrollbar
    set guioptions-=L                   " remove left GUI split scrollbar
    set guioptions-=r                   " remove right GUI scrollbar
    set guioptions-=R                   " remove right GUI split scrollbar
    set laststatus=2                    " always show bottom status bar
    set nocompatible                    " disable vi compatibility
    set smarttab                        " whitespace/tab stuff
    set wildmenu                        " tab completion
endif

set autoread                            " reads external file updates
set background=dark                     " default to goth background
set clipboard=unnamedplus               " enable clipboard access
set colorcolumn=80                      " mark column 80
set confirm                             " manage buffer state before exiting
set cursorline                          " show current line
set expandtab                           " set tabs = spaces
set foldcolumn=0                        " whitespace indentation on left margin
set formatoptions+=t                    " set textwidth for code; not comments
set guicursor=n:blinkon1                " blink cursor
set hidden                              " leave modded buffer
set history=100                         " history log
set ignorecase                          " ignore casing when searching
set list                                " ^
set listchars=tab:\\_,nbsp:_,trail:.    " make tabs + trailing spaces visible
set mouse=a                             " enable mouse
set nofoldenable                        " disable line folding
set number                              " line numbers
set ruler                               " show line+column at bottom right
set shell=zsh                           " shell = zsh
set shiftwidth=4                        " width of indent in spaces
set shortmess+=I                        " disable splash screen
set showcmd                             " show commands as typed
set smartcase                           " specify casing in searching
set softtabstop=4                       " whitespace/tab stuff
set spelllang=en_us                     " parse English
set splitbelow                          " always split windows below
set tabstop=4                           " width of tab char in spaces
set textwidth=0                         " textwidth = window width
set undolevels=500                      " extent of undo remembers
set visualbell                          " no beeping
set wildmode=list:longest,list          " tab completion

"""""""""""""""""""""
" automatic commands
"""""""""""""""""""""

""""""""""""
" by plugin
""""""""""""

" deoplete
" disable for prose
autocmd FileType text,tex,markdown let b:deoplete_disable_auto_complete = 1

" neomake
" async run neomake upon write
"autocmd! BufEnter,BufReadPost,BufWritePost * Neomake
autocmd! BufWritePost * Neomake

"""""""""""""""""""""
" by filetype, et al
"""""""""""""""""""""

" general
" remove all trailing whitespace upon write
autocmd BufWritePre * %s/\s\+$//e

" algol lang
" wrap curly braces and insert b/w
autocmd FileType c,c++,swift,javascript inoremap {<tab> {<cr>}<esc>O

augroup rainbow_algol
    autocmd!
    autocmd FileType c,cpp,rust,javascript RainbowParentheses
augroup END

" frontend
autocmd FileType html,css,scss setlocal noexpandtab

" func lang
autocmd FileType haskell setlocal expandtab tabstop=8 shiftwidth=4

augroup rainbow_func
    autocmd!
    autocmd FileType lisp,clojure,scheme,haskell RainbowParentheses
augroup END

" pep8
autocmd FileType python setlocal textwidth=80 tabstop=4 fileformat=unix

" txt
autocmd FileType text setlocal textwidth=80 spell
" autocmd BufWrite *.txt :normal ggVGgq`.zz

" viml
autocmd FileType vim setlocal textwidth=80 formatoptions+=t

""""""""""""
" functions
""""""""""""

function ToggleLazy()
    if(&lazyredraw == 0)
        set lazyredraw
        set cursorline!
    else
        set nolazyredraw
        set cursorline
    endif
endfunc

function ToggleLines()
    if(&relativenumber == 1)
        set norelativenumber
    else
        set relativenumber
    endif
endfunc

function LintSpell()
    set spell!
endfunc

""""""""""""""
" keybindings
""""""""""""""

" regular
inoremap jj <esc>|                          " rebind escape

nnoremap ; :|                               " rebind colon
nnoremap : ;|                               " rebind semicolon
nnoremap <c-e> $|                           " rebind end of line
nnoremap <c-f> <c-f>M|                      " center after page down.
nnoremap <c-b> <c-b>M|                      " center after page up.

vnoremap ; :|                               " rebind colon
vnoremap : ;|                               " rebind semicolon
vnoremap <c-e> $|                           " rebind end of line (visual)
vnoremap <c-f> <c-f>M|                      " center after page down.
vnoremap <c-b> <c-b>M|                      " center after page up.
vnoremap // y/<c-r>"<cr>|                   " search visual selection.

" leader
let mapleader = "\<space>"|                 " bind leader to spacebar

" leader general
noremap <leader>- <c-b>M|                   " page up
noremap <leader>= <c-f>M|                   " page down

" leader normal
nnoremap <leader>` :e $MYVIMRC<cr>|         " quick access to this file
nnoremap <leader>2 @@|                      " replay key macro q
nnoremap <leader>3 :!ctags -R .|            " make ctags in dir
nnoremap <leader>4 :call LintSpell()<cr>|   " call function
nnoremap <leader>5 %|                       " jump to matching delimeter
nnoremap <leader>7 :call ToggleLazy()<cr>|

nnoremap <leader>Q :qall<cr>|               " quit if everything is saved
nnoremap <leader>w :Buffers <cr>|           " fzf list buffer(s)
nnoremap <leader>y 0v$hy<cr>|               " yank a line without \n
nnoremap <leader>i 0i<cr><esc>k|            " insert line
nnoremap <leader>o :Files <cr>|             " fzf search/open child file(s)
nnoremap <leader>O :Files |                 " fzf search/open w/ given path
nnoremap <leader>[ <c-t>|                   " return from def ctag
nnoremap <leader>] <c-]>|                   " goto function def ctag

nnoremap <leader>f :Lines <cr>|             " fzf search thru open buffers
nnoremap <leader>s <c-w><c-w>|              " faster window jumping
nnoremap <leader>S :split<cr>|              " split new window below
nnoremap <leader>l `.zz|                    " jump to last edit and center
nnoremap <leader>L :call ToggleLines()<cr>|

nnoremap <leader>/ :noh<cr>|                " undo find highlighting

""""""""""""
" aesthetic
""""""""""""

if $COLORTERM == 'truecolor'
    set termguicolors                       " 24-bit-pretty
else
    set t_Co=256                            " ironic-pretty
endif

colorscheme vividchalk

if g:colors_name == "vividchalk"
    hi MatchParen ctermfg=none ctermbg=34 guifg=none guibg=#9933CC

    if system("uname -s") =~ "Linux"
        hi CursorLine term=bold cterm=bold
        hi LineNr ctermbg=235 guibg=#333333
    endif
endif

""""""""""""""""""
" plugin settings
""""""""""""""""""

" fzf
if system("uname -s") =~ "Darwin"
    set rtp+=/usr/local/opt/fzf
else
    set rtp+=~/.fzf
endif

let $FZF_DEFAULT_COMMAND = '
            \ rg --files --hidden --follow --no-ignore-vcs
            \ -g "!{.git,node_modules}/*" 2> /dev/null
            \ --ignore-file ~/.config/.rg_ignore '

let g:fzf_layout = { 'up': '~40%' }

" rainbow_parentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [
                       \['(', ')'],
                       \['[', ']'],
                       \['{', '}'],
                     \]

" vim-airline
let g:airline#extensions#tabline#enabled = 1
if g:colors_name == "cueva"
    let g:airline_theme = "cueva"
endif

""""""""""""""""""""
" language settings
""""""""""""""""""""

" haskell
let g:haskell_classic_highlighting = 1

" python
let python_highlight_all = 1                " better python syntax highlighting.
