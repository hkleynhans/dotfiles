-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  vim.cmd [[packadd packer.nvim]]
end

return require("packer").startup(function (use)
  use "wbthomason/packer.nvim"

  use { -- LSP
    "neovim/nvim-lspconfig",
    requires = {
      -- Automatically install LSPs
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",

      -- Useful status updates
      "j-hui/fidget.nvim",
    },
  }

  use { -- Autocompletion
    'hrsh7th/nvim-cmp',
    requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
  }

  use "TimUntersberger/neogit"

  use "nvim-lua/plenary.nvim"
  use "nvim-lua/popup.nvim"
  use "nvim-lua/telescope.nvim"

  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate"
  }

  use "editorconfig/editorconfig-vim"
  use "morhetz/gruvbox"
  use "navarasu/onedark.nvim" -- Theme inspired by Atom
  use "nvim-lualine/lualine.nvim" -- Fancier statusline

  if is_bootstrap then
    require('packer').sync()
  end
end)

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
