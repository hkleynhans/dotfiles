"
"
" Install minpac:
"
" Vim:
"
" git clone https://github.com/k-takata/minpac.git \
"    ~/.vim/pack/minpac/opt/minpac
"
" Neovim:
"
"    git clone https://github.com/k-takata/minpac.git \
"        ~/.config/nvim/pack/minpac/opt/minpac
"

set nocompatible                 " Use vim not vi
set exrc                         " enable per-directory .vimrc files
set secure                       " Disable unsafe commands

if !has('gui_running')
	set t_Co=256
endif

packadd minpac
call minpac#init()

call minpac#add('morhetz/gruvbox')
call minpac#add('tomasr/molokai')

call minpac#add('junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' })
call minpac#add('junegunn/fzf.vim')

call minpac#add('dense-analysis/ale')

call minpac#add('itchyny/lightline.vim')
call minpac#add('shinchu/lightline-gruvbox.vim')

call minpac#add('editorconfig/editorconfig-vim')

call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-dispatch')
call minpac#add('tpope/vim-fugitive')

call minpac#add('rhysd/vim-clang-format')

call minpac#add('qpkorr/vim-bufkill')

call minpac#add('benmills/vimux')

call minpac#add('fatih/vim-go')

" Syntactic language support
call minpac#add('cespare/vim-toml')
call minpac#add('stephpy/vim-yaml')
call minpac#add('plasticboy/vim-markdown')

packloadall
silent !helptags ALL

filetype plugin indent on

let g:go_fmt_command='goimports'
let g:go_auto_type_info=1

let g:airline#extensions#ale#enabled = 1

let g:ale_sign_column_always = 1

let g:fzf_command_prefix='Fzf'

let g:lightline = { 'colorscheme': 'gruvbox' }

let python_highlight_all=1

let g:vim_markdown_folding_disabled = 1

set autoread
set display+=lastline
set formatoptions+=j            " Delete comment char when joining commented lines

set autowrite                   " Automatically save if we are switching buffers
set backspace=indent,eol,start  " Allow backspacing over everything
set encoding=utf-8              " Prefer UTF-8 encoding

set spell                       " Enable spell-checking
set spelllang=en_gb             " Preferred dictionary

set ttyfast                     " Local terminal

set cursorline                  " Highlight the current line

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
set sidescrolloff=5             " More context around the cursor

set complete=.,b,u,]
set completeopt=menuone,menu,longest,preview

" Buffer area visuals
set visualbell                  " Use a visual bell, don't beep
" set cursorline                  " Highlight the current line
set lazyredraw

"set textwidth=79                " Text width of 79 characters
set colorcolumn=+1              " Highlight the column after textwidth

map <C-n> :cnext<CR>
map <C-p> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" Highlight tabs and trailing spaces
set listchars=tab:▸\ ,trail:•
set nolist                      " Make whitespace characters visible

" Colors
syn on
set background=dark
set t_Co=256                    " Enable 256 colors
colorscheme gruvbox

hi clear SpellBad
hi SpellBad cterm=underline ctermbg=52


" Search
set incsearch                   " Incrementally search
set hlsearch
set ignorecase                  " Ignore case when searching
set smartcase                   " Don't ignore case if I type a capital


" Tabs
set tabstop=8                   " Number of spaces to show for a tab
set shiftwidth=8                " Number of spaces to use for re-indent
"set softtabstop=4               " Number of spaces to insert when hitting tab
set autoindent                  " Put the cursor in the right place on newline
set smartindent                 " Put the cursor in the right place on newline
filetype plugin indent on

let mapleader = ' '
noremap <leader><F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<cr>
noremap <leader>e :FzfFiles<cr>
noremap <leader>f :FzfBuffers<cr>
noremap <leader>t :FzfTags<cr>
noremap <leader>b :Make<cr>
noremap <F7> :lprev<cr>
noremap <F8> :lnext<cr>


" Reformat the entire file - as opposed to re-indent.
" ggVG= (reindent)
" ggVGgq (reformat)
map <leader>i mzgg=G`z

" Look for a .clang-format file
let g:clang_format#detect_style_file=1

" map to <Leader>cf in C++ code
aug cpp
    au FileType c,cpp,objc nnoremap <buffer><Leader>c :<C-u>ClangFormat<CR>
    au FileType c,cpp,objc vnoremap <buffer><Leader>c :ClangFormat<CR>
    " au FileType c,cpp,objc ClangFormatAutoEnable
    au FileType c,cpp,objc set expandtab   " Insert spaces instead of tabs
aug END

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
  "au BufWritePost *.py call Flake8()
  au FileType python set expandtab           " Insert spaces instead of tabs
aug END

aug go
	au!
	au FileType go inoremap <buffer> . .<C-x><C-o>
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

