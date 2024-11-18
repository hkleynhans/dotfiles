--
--
--

-- Always set the leader first.
--
vim.keymap.set('n', '<Space>', '<Nop>', { silent=true })
vim.g.mapleader = ' '

-----------------------------------------------------------------------------
-- Shortcuts
-----------------------------------------------------------------------------

vim.keymap.set('', '<C-p>', '<cmd>Files<cr>')
vim.keymap.set('n', '<leader>;', '<cmd>Buffers<cr>')
vim.keymap.set('n', '<leader>v', vim.cmd.Ex)
vim.keymap.set('n', '<leader>c', '<cmd>e ~/AppData/Local/nvim/init.lua<cr>')

-----------------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------------

vim.o.hlsearch = false
vim.o.incsearch = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.signcolumn = "yes"

vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.softtabstop = 4

vim.o.colorcolumn = "+1"

vim.o.termguicolors = true

-----------------------------------------------------------------------------
-- Plugin manager
-----------------------------------------------------------------------------

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out, "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        {
            'tinted-theming/base16-vim',
            lazy = false,
            priority = 1000,
            config = function()
                vim.cmd.colorscheme('base16-horizon-dark')
            end
        },
        -- lualine
        {
            'nvim-lualine/lualine.nvim',
            dependencies = { 'nvim-tree/nvim-web-devicons' },
            config = function()
                require("lualine").setup({
                    options = {
                        icons_enabled = true,
                        theme = 'auto',
                    }
                })
            end
        },
        -- fzf
        {
            'junegunn/fzf.vim',
            dependencies = { 'junegunn/fzf' }
        },
        -- rust
        {
            'rust-lang/rust.vim',
            ft = { 'rust' },
            config = function()
                vim.g.rustfmt_autosave = 1
                vim.g.rustfmt_emit_files = 1
                vim.g.rustfmt_fail_silently = 0
                vim.g.rust_clip_command = 'wl-copy'
            end
        },
        {
            'morhetz/gruvbox',
            --config = function() 
            --    vim.cmd.colorscheme('gruvbox')
            --end
        },
        -- Nord color theme.
        {
            'shaunsingh/nord.nvim',
            --config = function()
            --    vim.g.nord_italic = true
            --    vim.g.nord_bold = true

            --    vim.cmd[[colorscheme nord]]
            --end
        },
        -- LSP
        {
            'neovim/nvim-lspconfig',
            config = function()
                local lspconfig = require('lspconfig')
                -- Clangd
                lspconfig.clangd.setup {}

                -- Rust
                lspconfig.rust_analyzer.setup {
                    settings = {
                        ["rust-analyzer"] = {
                            cargo = { allFeatures = true },
                            imports = { group = { enable = false } },
                            completion = { postfix = { enable = false } },
                        }
                    },
                    -- Inly hints are only useful if I can toggle it on and off.
                    --on_attach = function(client, bufnr) 
                        --  vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
                        --end,
                    }

                    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
                    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
                    vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
                    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)
                end,
            },
            -- LSP-based code completion
            {
                "hrsh7th/nvim-cmp",
                -- load on Insert Enter
                event = "InsertEnter",
                dependencies = {
                    'neovim/nvim-lspconfig',
                    'hrsh7th/cmp-nvim-lsp',
                    'hrsh7th/cmp-buffer',
                    'hrsh7th/cmp-path',
                },
                config = function() 
                    local cmp = require('cmp')
                    cmp.setup({
                        snippet = {
                            expand = function(args) 
                                vim.fn['vsnip#anonymous'](args.body)
                            end,
                        },
                        mapping = cmp.mapping.preset.insert({
                            ['<C-Space>'] = cmp.mapping.complete(),
                        }),
                        sources = cmp.config.sources({
                            { name = 'nvim_lsp' },
                        }, {
                            { name = 'path' },
                        }),
                    })

                    cmp.setup.cmdline(':', {
                        sources = cmp.config.sources({
                            { name = 'path' }
                        })
                    })
                end,
            },
            -- Inline function signature
            {
                "ray-x/lsp_signature.nvim",
                event = "VeryLazy",
                opts = {},
                config = function(_, opts)
                    require("lsp_signature").setup {}
                end
            },
        },
        -- automatically check for plugin updates
        checker = { enabled = true, notify = false },
    })


    --
    -- Neovide
    --
    if vim.g.neovide then
        vim.g.neovide_cursor_vfx_mode = ""
    end
