return {
  -- Neogit - Magit clone for Neovim
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "sindrets/diffview.nvim",        -- optional - Diff integration
      "nvim-telescope/telescope.nvim", -- optional - Telescope integration
    },
    cmd = "Neogit",
    keys = {
      { "<leader>kg", "", desc = "+git (neogit)" },
      { "<leader>kgg", "<cmd>Neogit<cr>", desc = "Open Neogit" },
      { "<leader>kgc", "<cmd>Neogit commit<cr>", desc = "Commit" },
      { "<leader>kgp", "<cmd>Neogit pull<cr>", desc = "Pull" },
      { "<leader>kgP", "<cmd>Neogit push<cr>", desc = "Push" },
      { "<leader>kgb", "<cmd>Neogit branch<cr>", desc = "Branch" },
      { "<leader>kgs", "<cmd>Neogit stash<cr>", desc = "Stash" },
      { "<leader>kgl", "<cmd>Neogit log<cr>", desc = "Log" },
      { "<leader>kgd", "<cmd>DiffviewOpen<cr>", desc = "Diff View" },
      { "<leader>kgD", "<cmd>DiffviewClose<cr>", desc = "Close Diff View" },
      { "<leader>kgh", "<cmd>DiffviewFileHistory %<cr>", desc = "File History" },
      { "<leader>kgH", "<cmd>DiffviewFileHistory<cr>", desc = "Branch History" },
    },
    opts = {
      -- Neogit configuration
      kind = "tab", -- Open in new tab (also: "split", "vsplit", "floating", "replace")

      -- Disable features you might not need
      disable_hint = false,
      disable_context_highlighting = false,
      disable_signs = false,

      -- Git configuration (updated format for Neogit 2026)
      git_services = {
        ["github.com"] = {
          pull_request = "https://github.com/${owner}/${repository}/compare/${branch_name}?expand=1",
          commit = "https://github.com/${owner}/${repository}/commit/${oid}",
          tree = "https://github.com/${owner}/${repository}/tree/${branch_name}",
        },
        ["gitlab.com"] = {
          pull_request = "https://gitlab.com/${owner}/${repository}/merge_requests/new?merge_request[source_branch]=${branch_name}",
          commit = "https://gitlab.com/${owner}/${repository}/-/commit/${oid}",
          tree = "https://gitlab.com/${owner}/${repository}/-/tree/${branch_name}?ref_type=heads",
        },
      },

      -- Telescope integration
      telescope_sorter = function()
        return require("telescope").extensions.fzf.native_fzf_sorter()
      end,

      -- Remember state across sessions
      remember_settings = true,

      -- Auto refresh
      auto_refresh = true,

      -- Commit popup
      commit_popup = {
        kind = "split",
      },

      -- Signs
      signs = {
        -- { CLOSED, OPENED }
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
      },

      integrations = {
        telescope = true,
        diffview = true,
      },
    },
  },

  -- Configure which-key groups
  {
    "folke/which-key.nvim",
    optional = true,
    opts = {
      spec = {
        { "<leader>kg", group = "git (neogit)", icon = "" },
      },
    },
  },
}
