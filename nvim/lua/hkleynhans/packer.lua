-- Install Packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  vim.cmd [[packadd packer.nvim]]
end

return require("packer").startup(function (use)
  use "wbthomason/packer.nvim"

  use {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    requires = {

      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {                                      -- Optional
        'williamboman/mason.nvim',
        run = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},     -- Required
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      {'hrsh7th/cmp-nvim-lsp'}, -- Required
      {'hrsh7th/cmp-nvim-lua'},
      {'L3MON4D3/LuaSnip'},     -- Required
    }
  }

  use { "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" }

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

  use "mfussenegger/nvim-dap"

  use "Civitasv/cmake-tools.nvim"
  use "rgroli/other.nvim"
  use "famiu/bufdelete.nvim"

  use "BurntSushi/ripgrep"

  if is_bootstrap then
      require("packer").sync()
  end
end)
