-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Scroll by viewing lines instead of actual lines
vim.keymap.set("n", "k", "gk", { noremap = true, silent = true })
vim.keymap.set("n", "j", "gj", { noremap = true, silent = true })
vim.keymap.set("n", "<Up>", "gk", { noremap = true, silent = true })
vim.keymap.set("n", "<Down>", "gj", { noremap = true, silent = true })

-- Tab for indenting
vim.keymap.set("n", "<Tab>", ">>_", { noremap = true, silent = true })
vim.keymap.set("n", "<S-Tab>", "<<_", { noremap = true, silent = true })
vim.keymap.set("i", "<S-Tab>", "<C-D>", { noremap = true, silent = true })
vim.keymap.set("v", "<Tab>", ">gv", { noremap = true, silent = true })
vim.keymap.set("v", "<S-Tab>", "<gv", { noremap = true, silent = true })

-- System clipboard macros (@y and @p registers)
vim.fn.setreg("p", '"+p')
vim.fn.setreg("y", '"+yy')

-- Create hash box with r#<space>
vim.keymap.set(
  "n",
  "r#<space>",
  '20I#<Esc>5a<Space><Esc>5A<Space><Esc>20A#<Esc>yy2P<C-V>$r#2j.',
  { noremap = true, silent = true }
)
