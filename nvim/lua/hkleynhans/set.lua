-- Set VIM options

vim.o.hlsearch = false

-- Line numbers default
vim.wo.number = true

-- Enable mouse
vim.o.mouse = 'a'

-- Save undo history
vim.o.undofile = true

-- Decrease the update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Case insensitive unless /C or capital in search string
vim.o.ignorecase = true
vim.o.smartcase = true

-- Colorscheme
vim.o.termguicolors = true
vim.cmd [[colorscheme gruvbox]]

-- Completion options
vim.o.completeopt = 'menuone,noselect'

-- Map Leader
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.o.pastetoggle = '<F2>'
vim.o.colorcolumn = tostring(80)   -- Line length marker

