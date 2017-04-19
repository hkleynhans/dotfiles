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
Plug 'vim-scripts/indentpython.vim'
Plug 'nvie/vim-flake8'
Plug 'editorconfig/editorconfig-vim'
Plug 'vim-syntastic/syntastic'
Plug 'python-mode/python-mode'
"Plug 'Valloric/YouCompleteMe'
Plug 'vim-ruby/vim-ruby'

filetype plugin indent on
call plug#end()

let g:fzf_command_prefix='Fzf'

let g:flake8_cmd="/bb/bigstorn/realtime_apps/IDEA/python/bin/flake8"
let python_highlight_all=1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


set backspace=indent,eol,start  " Allow backspacing over everything
set encoding=utf-8              " Prefer UTF-8 encoding
set spell                       " Enable spell-checking
set spelllang=en_gb             " Preferred dictionary

set ttyfast                     " Local terminal

set showmatch                   " Show matching brackets
set showmode                    " Show the current mode
set showcmd                     " Show the command as it is typed

set history=1000                " Remember more ex commands
set shortmess+=I                " Do not display a startup message
set shortmess+=A                " No .swp warning
set hidden                      " Allow hidden buffers (swap without writing)

"
" Visual elements
"
set laststatus=2                " The last window will have a status line always
set ruler                       " Show line and column numbers
set wildmenu                    " Command completion
set wildmode=list:longest       " List all matches and complete till longest
                                " common string
set scrolloff=3                 " More context around the cursor

set complete=.,b,u,]
set completeopt=menuone,menu,longest,preview

" Buffer area visuals
set visualbell                  " Use a visual bell, don't beep
set cursorline                  " Highlight the current line
set textwidth=79                " Text width of 79 characters
set colorcolumn=+1              " Highlight the column after textwidth


" Highlight tabs and trailing spaces
"set listchars=tab:▸\ ,trail:•
"set list                        " Make whitespace characters visible

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
set tabstop=4                   " Number of spaces to show for a tab
set shiftwidth=4                " Number of spaces to use for re-indent
set softtabstop=4               " Number of spaces to insert when hitting tab
set expandtab                   " Insert spaces instead of tabs
set autoindent                  " Put the cursor in the right place on newline
set smartindent                 " Put the cursor in the right place on newline
filetype plugin indent on

noremap <leader><F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<cr>
noremap <leader>e :FzfFiles<cr>
noremap <leader>t :FzfTags<cr>

map <leader>i mzgg=G`z

" automatically open and close the popup menu / preview window
"autocmd CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif


" Python
aug pygroup
  au!
  au FileType python setlocal tabstop=4
  au FileType python setlocal softtabstop=4
  au FileType python setlocal shiftwidth=4
  au FileType python setlocal textwidth=79
  au FileType python setlocal expandtab
  au FileType python setlocal autoindent
  au FileType python setlocal shiftround     " multiple of shiftwidth
  au FileType python setlocal fileformat=unix
  au BufWritePost *.py call Flake8()
aug END

" Highlight trailing spaces
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

set omnifunc=syntaxcomplete#Complete
au BufNewFile,BufRead,BufEnter *.cpp,*.h,*.hpp set omnifunc=omni#cpp#complete#Main

let OmniCpp_GlobalScopeSearch = 1    " Enable global scope search
let OmniCpp_ShowPrototypeInAbbr = 1  " Show function parameters
let OmniCpp_ShowAccess = 1           " Show access information in pop-up menu
let OmniCpp_MayCompleteDot = 1       " Auto complete after '.'
let OmniCpp_MayCompleteArrow = 1     " Auto complete after '->'
let OmniCpp_MayCompleteScope = 0     " Auto complete after '::'
let OmniCpp_SelectFirstItem = 0      " Don't select first item in pop-up menu

