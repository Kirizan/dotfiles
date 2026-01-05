return {
  -- Telescope - Fuzzy finder
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
  },

  -- FZF native sorter for telescope (C implementation for speed)
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
    config = function()
      -- Load the fzf extension
      require("telescope").load_extension("fzf")
    end,
  },
}
