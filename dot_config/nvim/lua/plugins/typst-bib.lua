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
        -- Configuration
        -- Note: Keybindings are set in typst.lua to match <leader>kT pattern
      })
    end,
  },
}
