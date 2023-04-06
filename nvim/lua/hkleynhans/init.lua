require('hkleynhans.packer')
require('hkleynhans.remap')
require('hkleynhans.set')

-- Set lualine as statusline
-- See `:help lualine.txt`
require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = 'gruvbox',
    component_separators = '|',
    section_separators = '',
  },
}

-- keymap
--vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
--vim.keymap.set('n', '<leader>f', require('telescope.builtin').find_files, { desc = 'Find existing [f]iles' })
--vim.keymap.set('n', '<leader>g', require('telescope.builtin').grep_string, { desc = 'Find current [w]ord' })

vim.keymap.set('n', '<F2>', ":set invpaste paste?<CR>")
vim.keymap.set('n', '<F7>', ":make<CR>")

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

-- treesitter
--require('nvim-treesitter.configs').setup {
--    ensure_installed = { 'c', 'cpp', 'go', 'rust', 'lua', 'python' },
--}

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

--require('mason').setup {}
--
--local servers = { 'clangd', 'rust_analyzer', 'pyright', 'sumneko_lua', 'gopls' }
--
--require('mason-lspconfig').setup {
--    ensure_installed = servers
--}
--
--local on_attach = function(_, bufnr)
--    local nmap = function(keys, func, desc)
--        if desc then
--	    desc = 'LSP: ' .. desc
--	end
--
--	vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
--    end
--
--    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
--    nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
--
--    -- See `:help K` for why this keymap
--    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
--    nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')
--
--    -- nmap('<leader>ff', vim.lsp.buf.format, 'Format document')
--end -- on_attach
--
--local cmp = require("cmp")
--cmp.setup({
--    preselect = cmp.PreselectMode.None,
--    mapping = {
--        ["<C-p>"] = cmp.mapping.select_prev_item(),
--        ["<C-n>"] = cmp.mapping.select_next_item(),
--        -- Add tab support
--        ["<S-Tab>"] = cmp.mapping.select_prev_item(),
--        ["<Tab>"] = cmp.mapping.select_next_item(),
--        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
--        ["<C-f>"] = cmp.mapping.scroll_docs(4),
--        ["<C-Space>"] = cmp.mapping.complete(),
--        ["<C-e>"] = cmp.mapping.close(),
--        ["<CR>"] = cmp.mapping.confirm({
--            behavior = cmp.ConfirmBehavior.Insert,
--            select = true,
--        }),
--    },
--    sources = {
--    	{ name = "nvim_lsp" },
--	{ name = "path" },
--	{ name = "buffer" },
--    },
--})
--
---- nvim-cmp supports additional completion capabilities
--local capabilities = vim.lsp.protocol.make_client_capabilities()
--capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
--
--for _, lsp in ipairs(servers) do
--    require('lspconfig')[lsp].setup {
--        on_attach = on_attach,
--        capabilities = capabilities,
--    }
--end

-- require('fidget').setup {}

-- Example custom configuration for lua
--
-- Make runtime files discoverable to the server
--local runtime_path = vim.split(package.path, ';')
--table.insert(runtime_path, 'lua/?.lua')
--table.insert(runtime_path, 'lua/?/init.lua')
--
--require('lspconfig').sumneko_lua.setup {
--  on_attach = on_attach,
--  capabilities = capabilities,
--  settings = {
--    Lua = {
--      runtime = {
--        -- Tell the language server which version of Lua you're using (most likely LuaJIT)
--        version = 'LuaJIT',
--        -- Setup your lua path
--        path = runtime_path,
--      },
--      diagnostics = {
--        globals = { 'vim' },
--      },
--      workspace = { library = vim.api.nvim_get_runtime_file('', true) },
--      -- Do not send telemetry data containing a randomized but unique identifier
--      telemetry = { enable = false },
--    },
--  },
--}
