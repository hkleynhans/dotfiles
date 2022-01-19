----------------------------------- ALIASES -----------------------------------
local api, cmd, fn, g, opt = vim.api, vim.cmd, vim.fn, vim.g, vim.opt

local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  api.nvim_set_keymap(mode, lhs, rhs, options)
end

----------------------------------- PLUGINS -----------------------------------
--
-- Use the 'paq' package manager as it is natively written in lua.  To install
-- it:
--
-- $ git clone https://github.com/savq/paq-nvim.git \
--    "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/pack/paqs/opt/paq-nvim
--

cmd 'packadd paq-nvim'
local paq = require('paq-nvim').paq
paq {'savq/paq-nvim', opt = true}

paq {'neovim/nvim-lspconfig'}

paq {'shougo/deoplete-lsp'}
paq {'shougo/deoplete.nvim', run = fn['remote#host#UpdateRemotePlugins']}

paq {'nvim-treesitter/nvim-treesitter'}
paq {'junegunn/fzf', run = fn['fzf#install']}
paq {'junegunn/fzf.vim'}
paq {'morhetz/gruvbox'}
paq {'Mofiqul/dracula.nvim'}
paq {'ojroques/nvim-lspfuzzy'}

paq {'editorconfig/editorconfig-vim'}

paq {'mfussenegger/nvim-dap'}
paq {'rcarriga/nvim-dap-ui'}
paq {'theHamsta/nvim-dap-virtual-text'}

paq {'rhysd/vim-clang-format'}

-- paq {'ray-x/go.nvim'}
paq {'fatih/vim-go'}

paq {'rust-lang/rust.vim'}
paq {'simrat39/rust-tools.nvim'}

paq {'nvim-lua/popup.nvim'}
paq {'nvim-lua/plenary.nvim'}
paq {'nvim-telescope/telescope.nvim'}


g['deoplete#enable_at_startup'] = 1
g['rustfmt_autosave'] = 1

-- gruvbox seems to be the better theme.
cmd 'colorscheme gruvbox'
-- cmd 'colorscheme dracula'

---------------------------------- OPTIONS ------------------------------------
local indent, tabstop, width = 4, 8, 80
opt.cindent = true
opt.colorcolumn = tostring(width)   -- Line length marker
opt.completeopt = {'menuone', 'noinsert', 'noselect'}  -- Completion options
opt.cursorline = true               -- Highlight cursor line
opt.expandtab = true                -- Use spaces instead of tabs
opt.formatoptions = 'crqnj'         -- Automatic formatting options
opt.hidden = true                   -- Enable background buffers
opt.ignorecase = true               -- Ignore case
opt.joinspaces = false              -- No double spaces with join
opt.list = true                     -- Show some invisible characters
opt.number = true                   -- Show line numbers
opt.pastetoggle = '<F2>'            -- Paste mode
opt.pumheight = 12                  -- Max height of popup menu
opt.relativenumber = true           -- Relative line numbers
opt.scrolloff = 8                   -- Lines of context
opt.shiftround = true               -- Round indent
opt.shiftwidth = indent             -- Size of an indent
opt.shortmess = 'atToOFc'           -- Prompt message options
opt.sidescrolloff = 8               -- Columns of context
opt.signcolumn = 'yes'              -- Show sign column
opt.smartcase = true                -- Do not ignore case with capitals
opt.smartindent = true              -- Insert indents automatically
opt.splitbelow = true               -- Put new windows below current
opt.splitright = true               -- Put new windows right of current
opt.tabstop = tabstop               -- Number of spaces tabs count for
opt.termguicolors = true            -- True color support
opt.textwidth = width               -- Maximum width of text
opt.updatetime = 100                -- Delay before swap file is saved
opt.wildmode = {'list', 'longest'}  -- Command-line completion mode
opt.wrap = false                    -- Disable line wrap

g.mapleader = ' '

----------------------- Edit and Reload configuration -------------------------

map('n', '<leader>ve', '<cmd>edit ~/.config/nvim/init.lua<CR>')
map('n', '<leader>vr', '<cmd>source ~/.config/nvim/init.lua<CR>')

---------------------------------- TREE-SITTER --------------------------------
local ts = require 'nvim-treesitter.configs'
ts.setup {ensure_installed = 'maintained', highlight = {enable = true}}

---------------------------------- GO -----------------------------------------
g["go_highlight_types"] = 1
g["go_highlight_fields"] = 1
g["go_highlight_functions"] = 1
g["go_highlight_function_calls"] = 1
g["go_highlight_operators"] = 1
g["go_highlight_extra_types"] = 1
g["go_highlight_build_constraints"] = 1
g["go_highlight_generate_tags"] = 1

local go = require 'go'
go.setup{
    verbose = false,

    dap_debug = true,
    dap_debug_keymap = false,
    dap_debug_gui = true,
    dap_debug_vt = true,

    lsp_cfg = false,
    lsp_fofumpt = false,
    lsp_on_attach = false,
    gopls_cmd = nil,
}

cmd([[
augroup golang
    au!
    au BufEnter *.go set et
    au BufEnter *.go nmap <leader>t  <Plug>(go-test)
    au BufEnter *.go nmap <leader>tt <Plug>(go-test-func)
    au BufEnter *.go nmap <leader>c  <Plug>(go-coverage-toggle)
    au BufEnter *.go nmap <leader>i  <Plug>(go-info)
    au BufEnter *.go nmap <leader>ii <Plug>(go-implements)
    au BufEnter *.go nmap <leader>ci <Plug>(go-describe)
    au BufEnter *.go nmap <leader>cc <Plug>(go-callers)
augroup END
]])

---------------------------------- LSP ----------------------------------------
-- require('go').setup()

local lsp = require 'lspconfig'
local lspfuzzy = require 'lspfuzzy'
local rust_tools = require 'rust-tools'

-- We use the default settings for ccls and pylsp: the option table can stay empty
rust_tools.setup {}
lsp.ccls.setup {}
lsp.gopls.setup {}

-- Not sure which of these python lsp servers to use.
-- lsp.pylsp.setup {}
-- lsp.pyls.setup {}
lsp.rust_analyzer.setup {
  on_attach = on_attach,
  settings = {
    ["rust-analyzer"] = {
      assist = {
        importMergeBehavior = "last",
        importPrefix = "by_self",
      },
      diagnostics = {
        disabled = { "unresolved-import" }
      },
      cargo = {
          loadOutDirsFromCheck = true
      },
      procMacro = {
          enable = true
      },
      checkOnSave = {
          command = "clippy"
      },
    }
  }
}

lspfuzzy.setup {}  -- Make the LSP client use FZF instead of the quickfix list

map('n', '<leader>n', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
map('n', '<leader>p', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
map('n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>')
map('n', '<leader>D', '<cmd>lua vim.lsp.buf.declaration()<CR>')
map('n', '<leader>d', '<cmd>lua vim.lsp.buf.definition()<CR>')
map('n', '<leader>i', '<cmd>lua vim.lsp.buf.formatting()<CR>')
map('n', '<leader>h', '<cmd>lua vim.lsp.buf.hover()<CR>')
map('n', '<leader>m', '<cmd>lua vim.lsp.buf.rename()<CR>')
map('n', '<leader>r', '<cmd>lua vim.lsp.buf.references()<CR>')
map('n', '<leader>s', '<cmd>lua vim.lsp.buf.document_symbol()<CR>')

---------------------------------- MAPPINGS -----------------------------------

-- Compile
map('n', '<leader>c', '<cmd>make<CR>')

-- fzf
g.fzf_command_prefix='Fzf'
map('n', '<leader>b', '<cmd>FzfBuffers<CR>')
map('n', '<leader>f', '<cmd>FzfFiles<CR>')
map('n', '<leader>t', '<cmd>FzfTags<CR>')

-- ClangFormat.  May need to configure per mode.
map('n', '<leader>=', '<cmd>ClangFormat<CR>')

-- Once we press enter, the search should is done.
cmd 'set nohlsearch'

-------------------------------- Experimental ---------------------------------
--local sumneko_binary_path = vim.fn.exepath('lua-language-server')
--local sumneko_root_path = vim.fn.fnamemodify(sumneko_binary_path, ':h:h:h')

--local runtime_path = vim.split(package.path, ';')
--table.insert(runtime_path, "lua/?.lua")
--table.insert(runtime_path, "lua/?/init.lua")
--
--lsp.sumneko_lua.setup {
--  cmd = {sumneko_binary_path, "-E", sumneko_root_path .. "/main.lua"};
--  settings = {
--    Lua = {
--      runtime = {
--        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--        version = 'LuaJIT',
--        -- Setup your lua path
--        path = runtime_path,
--      },
--      diagnostics = {
--        -- Get the language server to recognize the `vim` global
--        globals = {'vim'},
--      },
--      workspace = {
--        -- Make the server aware of Neovim runtime files
--        library = vim.api.nvim_get_runtime_file("", true),
--      },
--      -- Do not send telemetry data containing a randomized but unique identifier
--      telemetry = {
--        enable = false,
--      },
--    },
--  },
--}

