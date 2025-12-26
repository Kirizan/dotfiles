-- Typst Bibliography Management Plugin
-- https://github.com/Kirizan/typst-bib.nvim

return {
  {
    "Kirizan/typst-bib.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
    },
    ft = "typst",
    opts = {
      -- Optional: customize configuration
      -- entry_types = { "online", "article", "book", "inproceedings", "thesis", "misc" },
      -- default_bib_dir = "references",
    },
    keys = {
      { "<leader>TR", "<cmd>TypstBibAdd<cr>", desc = "Add Reference", ft = "typst" },
      { "<leader>Tr", "<cmd>TypstBibInsert<cr>", desc = "Insert Reference", ft = "typst" },
    },
  },

  -- Add which-key descriptions
  {
    "folke/which-key.nvim",
    opts = function(_, opts)
      local wk = require("which-key")
      wk.add({
        { "<leader>TR", desc = "Add Reference" },
        { "<leader>Tr", desc = "Insert Reference" },
      })
    end,
  },
}
