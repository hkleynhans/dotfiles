"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/hkleynhans/.dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/Users/hkleynhans/.dein')
  call dein#begin('/Users/hkleynhans/.dein')

  " Let dein manage dein
  " Required:
  call dein#add('/Users/hkleynhans/.dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('zchee/deoplete-jedi')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  call dein#add('tomasr/molokai')

  call dein#add('junegunn/fzf', { 'build': './install -all', 'merged':0 })
  call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

"End dein Scripts-------------------------

call deoplete#enable()

" Fzf
let g:fzf_command_prefix='Fzf'

noremap <leader>e :FzfFiles<cr>
noremap <leader>t :FzfTags<cr><Paste>

" Colorscheme
let g:molokai_original=1
let g:rehash256=1
colorscheme molokai

set visualbell			" no audible bell
set cursorline			" highlight the current line
set textwidth=79
set colorcolumn=+1		" Highlight column
