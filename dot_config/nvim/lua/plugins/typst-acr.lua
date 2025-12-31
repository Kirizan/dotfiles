-- Typst Acronym Management Plugin
-- https://github.com/Kirizan/typst-acr.nvim

return {
  {
    "Kirizan/typst-acr.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-telescope/telescope.nvim",
    },
    ft = "typst",
    config = function()
      require("typst-acr").setup({
        -- Configuration
        -- Note: Keybindings are set in typst.lua under <leader>kT (uppercase T)

        -- Use default settings with global library enabled
        global_library = {
          enabled = true,
          path = vim.fn.stdpath("data") .. "/typst-acr/global-acronyms.typ",
        },

        -- Default cycle sequence
        cycle_sequence = {
          "@%s",         -- @cpu
          "@%s:short",   -- @cpu:short
          "@%s:long",    -- @cpu:long
          "@%s:pl",      -- @cpu:pl
        },
      })
    end,
  },
}
