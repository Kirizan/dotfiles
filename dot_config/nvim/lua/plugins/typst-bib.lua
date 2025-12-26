-- Typst Bibliography Management Plugin
-- https://github.com/Kirizan/typst-bib.nvim

return {
  {
    "Kirizan/typst-bib.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
    },
    ft = "typst",
    config = function()
      require("typst-bib").setup({
        -- Optional: customize configuration
        -- entry_types = { "online", "article", "book", "inproceedings", "thesis", "misc" },
        -- default_bib_dir = "references",
      })
    end,
  },
}
