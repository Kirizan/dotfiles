-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- Tab settings (2 spaces)
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0
vim.opt.shiftround = true

-- Status line
vim.opt.laststatus = 2

-- Font
vim.opt.guifont = "Menlo Regular:h18"

-- Word wrap
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.breakat = vim.o.breakat

-- Cursor timeout
vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 1
vim.opt.ttyfast = true
