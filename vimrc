"
"
" Install Plug:
"
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"

set nocompatible                 " Use vim not vi
set exrc                         " enable per-directory .vimrc files
set secure                       " Disable unsafe commands

call plug#begin('~/.vim/plugged')

Plug 'tomasr/molokai'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'python-mode/python-mode'
"Plug 'Valloric/YouCompleteMe'

filetype plugin indent on
call plug#end()

let g:fzf_command_prefix='Fzf'

set backspace=indent,eol,start  " Allow backspacing over everything
set encoding=utf-8              " Prefer UTF-8 encoding
set spell                       " Enable spell-checking
set spelllang=en_gb             " Preferred dictionary

set ttyfast                     " Local terminal

set showmatch                   " Show matching brackets
set showmode                    " Show the current mode
set showcmd                     " Show the command as it is typed

set history=200                 " Remember more ex commands
set shortmess+=I                " Do not display a startup message
set shortmess+=A                " No .swp warning
set nohidden                    " Don't allow hidden buffers

"
" Visual elements
"
set laststatus=2                " The last window will have a status line always
set ruler                       " Show line and column numbers
set wildmenu                    " Command completion
set wildmode=list:longest       " List all matches and complete till longest
                                " common string

" Buffer area visuals
set visualbell                  " Use a visual bell, don't beep
set cursorline                  " Highlight the current line
set textwidth=79                " Text width of 79 characters
set colorcolumn=+1              " Highlight the column after textwidth


" Highlight tabs and trailing spaces
set listchars=tab:▸\ ,trail:•
set list                        " Make whitespace characters visible

" Colors
syn on
set background=dark
set t_Co=256                    " Enable 256 colors
colorscheme molokai

" Search
set incsearch                   " Incrementally search
set hlsearch
set ignorecase                  " Ignore case when searching
set smartcase                   " Don't ignore case if I type a capital


" Tabs
set tabstop=2                   " Number of spaces to show for a tab
set shiftwidth=2                " Number of spaces to use for re-indent
set softtabstop=4               " Number of spaces to insert when hitting tab
set expandtab                   " Insert spaces instead of tabs
set autoindent                  " Put the cursor in the right place on newline
filetype plugin indent on

noremap <leader><F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<cr>
noremap <leader>e :FzfFiles<cr>
noremap <leader>t :FzfTags<cr>

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

