return {
  -- obsidian.nvim: Obsidian vault integration for Neovim
  {
    "obsidian-nvim/obsidian.nvim",
    version = "*",
    lazy = true,
    ft = "markdown",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      ui = { enable = false }, -- render-markdown.nvim handles rendering
      workspaces = {
        { name = "primary", path = "~/Obsidian/Primary" },
        { name = "work", path = "~/Obsidian/Work" },
        { name = "sandbox", path = "~/Obsidian/Sandbox" },
      },
      completion = { nvim_cmp = false }, -- markdown-oxide LSP handles completions
      daily_notes = {
        folder = "daily",
        date_format = "%Y-%m-%d",
      },
      templates = {
        folder = "templates",
      },
      picker = {
        name = "telescope.nvim",
      },
      -- Don't auto-create frontmatter — let templates handle it
      disable_frontmatter = true,
    },
  },

  -- Keybindings for Obsidian (under <leader>ko)
  {
    "LazyVim/LazyVim",
    opts = function()
      -- Register which-key group globally
      local wk = require("which-key")
      wk.add({
        { "<leader>ko", group = "obsidian", icon = "󱓧" },
      })

      -- Obsidian-specific keybindings (only active in markdown files)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "markdown",
        callback = function()
          local buf = vim.api.nvim_get_current_buf()

          -- <leader>kon - New note
          vim.keymap.set("n", "<leader>kon", function()
            vim.cmd("ObsidianNew")
          end, { buffer = buf, desc = "New Note" })

          -- <leader>kod - Today's daily note
          vim.keymap.set("n", "<leader>kod", function()
            vim.cmd("ObsidianToday")
          end, { buffer = buf, desc = "Daily Note" })

          -- <leader>kos - Search vault (Telescope)
          vim.keymap.set("n", "<leader>kos", function()
            vim.cmd("ObsidianSearch")
          end, { buffer = buf, desc = "Search Vault" })

          -- <leader>kob - Backlinks
          vim.keymap.set("n", "<leader>kob", function()
            vim.cmd("ObsidianBacklinks")
          end, { buffer = buf, desc = "Backlinks" })

          -- <leader>kot - Insert template
          vim.keymap.set("n", "<leader>kot", function()
            vim.cmd("ObsidianTemplate")
          end, { buffer = buf, desc = "Insert Template" })

          -- <leader>kor - Rename note (updates all backlinks)
          vim.keymap.set("n", "<leader>kor", function()
            vim.cmd("ObsidianRename")
          end, { buffer = buf, desc = "Rename Note" })

          -- <leader>koo - Open in Obsidian app
          vim.keymap.set("n", "<leader>koo", function()
            vim.cmd("ObsidianOpen")
          end, { buffer = buf, desc = "Open in Obsidian" })

          -- <leader>kog - Tags search
          vim.keymap.set("n", "<leader>kog", function()
            vim.cmd("ObsidianTags")
          end, { buffer = buf, desc = "Search Tags" })

          -- Register which-key descriptions for this buffer
          wk.add({
            { "<leader>kon", desc = "New Note", buffer = buf },
            { "<leader>kod", desc = "Daily Note", buffer = buf },
            { "<leader>kos", desc = "Search Vault", buffer = buf },
            { "<leader>kob", desc = "Backlinks", buffer = buf },
            { "<leader>kot", desc = "Insert Template", buffer = buf },
            { "<leader>kor", desc = "Rename Note", buffer = buf },
            { "<leader>koo", desc = "Open in Obsidian", buffer = buf },
            { "<leader>kog", desc = "Search Tags", buffer = buf },
          })
        end,
      })
    end,
  },
}
