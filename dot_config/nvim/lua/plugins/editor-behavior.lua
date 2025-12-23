-- Editor behavior configuration
-- Auto-save, persistent undo, and other editor settings

return {
  {
    "LazyVim/LazyVim",
    opts = function()
      -- ============================================================================
      -- Auto-save configuration
      -- ============================================================================
      -- Auto-save files when they are modified (helps with typst-preview updates)
      vim.opt.autowrite = true -- Auto-save before commands like :next, :make, etc.
      vim.opt.autowriteall = true -- Auto-save on more events (buffer switch, etc.)

      -- Auto-save on text change with a delay (like VSCode)
      local auto_save_group = vim.api.nvim_create_augroup("AutoSave", { clear = true })
      vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
        group = auto_save_group,
        pattern = "*",
        callback = function()
          if vim.fn.empty(vim.fn.bufname("%")) == 0 and vim.bo.modified then
            -- Debounce: only save after 1 second of inactivity
            vim.defer_fn(function()
              if vim.bo.modified and vim.bo.buftype == "" and vim.fn.filereadable(vim.fn.expand("%")) == 1 then
                -- Use noautocmd to prevent format-on-save and whitespace trimming
                vim.cmd("silent! noautocmd write")
                -- Optional: show a subtle notification
                -- vim.notify("File auto-saved", vim.log.levels.INFO, { timeout = 500 })
              end
            end, 1000)
          end
        end,
      })

      -- ============================================================================
      -- Persistent undo configuration (infinite undo history)
      -- ============================================================================
      -- Enable persistent undo (survives Neovim restarts)
      vim.opt.undofile = true

      -- Set undo directory (LazyVim already sets this to ~/.local/state/nvim/undo)
      -- But we'll ensure it exists and is configured
      local undo_dir = vim.fn.stdpath("state") .. "/undo"
      vim.fn.mkdir(undo_dir, "p") -- Create directory if it doesn't exist
      vim.opt.undodir = undo_dir

      -- Set undo levels to a very high number (essentially infinite)
      vim.opt.undolevels = 10000 -- Maximum number of changes that can be undone
      vim.opt.undoreload = 10000 -- Maximum number of lines to save for undo on buffer reload

      -- ============================================================================
      -- Additional editor improvements
      -- ============================================================================
      -- Save cursor position and restore it when reopening files
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern = "*",
        callback = function()
          local line = vim.fn.line("'\"")
          if line > 0 and line <= vim.fn.line("$") then
            vim.cmd('normal! g`"')
          end
        end,
      })

      -- Highlight yanked text briefly
      vim.api.nvim_create_autocmd("TextYankPost", {
        pattern = "*",
        callback = function()
          vim.highlight.on_yank({ higroup = "IncSearch", timeout = 200 })
        end,
      })
    end,
  },
}
