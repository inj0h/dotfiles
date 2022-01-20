" Filename: dotvim.vim
" Note:     Lean. Mean. Vim config.

" 00. General
syntax off
filetype on
filetype plugin on
filetype plugin indent on
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

" 01. Indentation
set autoindent
set expandtab
set shiftwidth=4
set tabstop=4

" 02. Keybindings
inoremap hh <esc>
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" 03. Status Bar
set laststatus=2
set ruler
set noshowcmd
set statusline=                        " Reset
set statusline+=\ %.40f                " Relative filepath (40 char max width)
set statusline+=\%m                    " If modified
set statusline+=\ %y                   " Filetype
set statusline+=%=                     " Spacer
set statusline+=\%l:%c                 " Line:column numbers
set statusline+=\ \ \ \ \ \ \ \ \ %P\  " Buffer percentage

" 04. Hooks
augroup plainText
    autocmd!
    autocmd FileType gitcommit setlocal
                \ spell
                \ textwidth=72
    autocmd FileType markdown,tex,text setlocal spell
augroup END
