return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    opts = {
      -- Size of terminal window
      size = function(term)
        if term.direction == "horizontal" then
          return 15
        elseif term.direction == "vertical" then
          return vim.o.columns * 0.4
        end
      end,
      open_mapping = [[<c-\>]], -- Ctrl+\ to toggle terminal
      hide_numbers = true, -- Hide line numbers in terminal
      shade_terminals = true,
      start_in_insert = true,
      insert_mappings = true, -- Apply open_mapping in insert mode
      terminal_mappings = true, -- Apply open_mapping in terminal mode
      persist_size = true,
      persist_mode = true, -- Persist terminal mode (insert vs normal)
      direction = "float", -- 'vertical' | 'horizontal' | 'tab' | 'float'
      close_on_exit = true, -- Close terminal window when process exits
      shell = vim.o.shell, -- Use default shell
      float_opts = {
        border = "curved",
        winblend = 0,
        highlights = {
          border = "Normal",
          background = "Normal",
        },
      },
    },
    config = function(_, opts)
      require("toggleterm").setup(opts)

      -- Custom terminal keybindings under <leader>kt (KirDoIt -> terminal)
      local Terminal = require("toggleterm.terminal").Terminal

      -- Horizontal terminal
      vim.keymap.set("n", "<leader>kth", function()
        vim.cmd("ToggleTerm direction=horizontal")
      end, { desc = "Toggle Horizontal Terminal" })

      -- Vertical terminal
      vim.keymap.set("n", "<leader>ktv", function()
        vim.cmd("ToggleTerm direction=vertical")
      end, { desc = "Toggle Vertical Terminal" })

      -- Floating terminal (default with Ctrl+\)
      vim.keymap.set("n", "<leader>ktf", function()
        vim.cmd("ToggleTerm direction=float")
      end, { desc = "Toggle Floating Terminal" })

      -- Terminal in new tab
      vim.keymap.set("n", "<leader>ktt", function()
        vim.cmd("ToggleTerm direction=tab")
      end, { desc = "Toggle Terminal in Tab" })

      -- Kill terminal
      vim.keymap.set("n", "<leader>ktk", function()
        vim.cmd("ToggleTermToggleAll")
      end, { desc = "Toggle All Terminals" })

      -- Terminal mode mappings (for escaping and navigating)
      function _G.set_terminal_keymaps()
        local opts = { buffer = 0 }
        vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts) -- Escape to normal mode
        vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts) -- Navigate left
        vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts) -- Navigate down
        vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts) -- Navigate up
        vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts) -- Navigate right
      end

      -- Apply terminal keymaps when entering terminal buffer
      vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

      -- Which-key integration
      local wk = require("which-key")
      wk.add({
        { "<leader>kt", group = "terminal" },
        { "<leader>kth", desc = "Horizontal Terminal" },
        { "<leader>ktv", desc = "Vertical Terminal" },
        { "<leader>ktf", desc = "Floating Terminal" },
        { "<leader>ktt", desc = "Terminal in Tab" },
        { "<leader>ktk", desc = "Toggle All Terminals" },
      })
    end,
  },
}
