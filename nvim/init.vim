" filename:         init.vim
" description:
"                   Personal (neo)vim settings.
"

"
" plugins
"

call plug#begin('~/.config/nvim/plugs')

Plug 'erikorojo/cueva'
Plug 'hdima/python-syntax'
Plug 'hynek/vim-python-pep8-indent'
Plug 'junegunn/fzf.vim', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
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

"
" general
"

syntax on
filetype on
filetype plugin on
filetype plugin indent on

" turn these on if using regular vim and not nvim
if !has('nvim')
    set autoindent
    set backspace=eol,start,indent
    set guioptions-=l guioptions-=L guioptions-=r guioptions-=R
    set hlsearch
    set laststatus=2
    set nocompatible
    set smarttab
    set wildmenu
endif

set autoread
set background=dark
set clipboard=unnamedplus
set colorcolumn=80
set confirm
set cursorline
set expandtab
set foldcolumn=0
set formatoptions+=t
set guicursor=n:blinkon1
set hidden
set history=100
set ignorecase
set list
set listchars=tab:>\ ,nbsp:_,trail:.
set mouse=a
set nofoldenable
set number
set ruler
set shell=zsh
set shiftwidth=4
set shortmess+=I
set showcmd
set smartcase
set softtabstop=4
set spelllang=en_us
set splitbelow
set tabstop=4
set textwidth=0
set undolevels=500
set visualbell
set wildmode=list:longest,list

"
" automatic commands
"

"
" by plugin
"

" deoplete
" disable for prose
au FileType text call deoplete#custom#option('auto_complete', v:false)

" neomake
" async run neomake upon write
"au! BufEnter,BufReadPost,BufWritePost * Neomake
au! BufWritePost * Neomake

"
" by filetype, et al
"

" general
" remove all trailing whitespace upon write
au BufWritePre * %s/\s\+$//e

" algol lang
" wrap curly braces and insert b/w
au FileType c,c++,swift,javascript inoremap {<tab> {<cr>}<esc>O

aug rainbow_algol
    au!
    au FileType c,cpp,rust,javascript RainbowParentheses
aug END

" frontend
au FileType html,css,scss setlocal noexpandtab

" func lang
au FileType haskell setlocal expandtab tabstop=8 shiftwidth=4

aug rainbow_func
    au!
    au FileType lisp,clojure,scheme,haskell RainbowParentheses
aug END

" git commits
au FileType gitcommit setlocal textwidth=72 spell

" pep8
au FileType python setlocal textwidth=80 tabstop=4 fileformat=unix

" txt
au FileType text setlocal textwidth=80 spell

" viml
au FileType vim setlocal textwidth=80 formatoptions+=t

"
" functions
"

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

"
" keybindings
"

" regular
inoremap hh <esc>|                          " rebind escape

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
nnoremap <leader>[ <c-t>|                   " return from def ctag
nnoremap <leader>] <c-]>|                   " goto function def ctag

nnoremap <leader>y 0v$hy<cr>|               " yank a line without \n
nnoremap <leader>f :Lines <cr>|             " fzf search thru open buffers
nnoremap <leader>l `.zz|                    " jump to last edit and center
nnoremap <leader>L :call ToggleLines()<cr>|
nnoremap <leader>/ :noh<cr>|                " undo find highlighting

nnoremap <leader>o :Files <cr>|             " fzf search/open child file(s)
nnoremap <leader>O :Files |                 " fzf search/open w/ given path
nnoremap <leader>i 0i<cr><esc>k|            " insert line
nnoremap <leader>s <c-w><c-w>|              " faster window jumping

nnoremap <leader>w :Buffers <cr>|           " fzf list buffer(s)

"
" aesthetic
"

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

"
" plugin settings
"

" fzf
if system("uname -s") =~ "Darwin"
    set rtp+=/usr/local/opt/fzf
else
    set rtp+=~/.fzf
endif

let $FZF_DEFAULT_COMMAND = '
            \ rg --files --hidden --follow --no-ignore-vcs
            \ -g "!{.git,node_modules}/*" 2> /dev/null
            \ --ignore-file ~/.config/.rgignore '

let g:fzf_layout = { 'up': '~40%' }

" rainbow_parentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [
                       \['(', ')'],
                       \['[', ']'],
                       \['{', '}'],
                     \]

" vim-airline
if g:colors_name == "cueva"
    let g:airline_theme = "cueva"
endif

"
" language settings
"

" haskell
let g:haskell_classic_highlighting = 1

" python
let python_highlight_all = 1                " better python syntax highlighting.
