return {
  -- Typst language support with LSP (tinymist)
  {
    "chomosuke/typst-preview.nvim",
    lazy = false, -- Load immediately for .typ files
    version = "0.3.*",
    build = function()
      require("typst-preview").update()
    end,
    ft = "typst",
    opts = {
      -- Configuration for typst-preview
      dependencies_bin = {
        -- Use system typst and tinymist from Homebrew
        ["typst-preview"] = nil, -- Will use bundled version
        ["websocat"] = nil, -- Will use bundled version
      },
    },
  },

  -- Typst LSP configuration via Mason
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        tinymist = {
          settings = {
            exportPdf = "onSave",
            formatterMode = "typstyle",
          },
        },
      },
    },
  },

  -- Ensure tinymist is installed via Mason
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "tinymist",
      },
    },
  },

  -- Keybindings for Typst
  {
    "LazyVim/LazyVim",
    opts = function(_, opts)
      -- Typst-specific keybindings under <leader>kt
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "typst",
        callback = function()
          local buf = vim.api.nvim_get_current_buf()

          -- <leader>ktp - Toggle preview
          vim.keymap.set("n", "<leader>ktp", function()
            require("typst-preview").toggle()
          end, { buffer = buf, desc = "Toggle Typst Preview" })

          -- <leader>kts - Sync preview (jump to current position)
          vim.keymap.set("n", "<leader>kts", function()
            require("typst-preview").sync_with_cursor()
          end, { buffer = buf, desc = "Sync Typst Preview" })

          -- <leader>ktc - Compile to PDF
          vim.keymap.set("n", "<leader>ktc", function()
            local file = vim.fn.expand("%:p")
            local output = vim.fn.expand("%:p:r") .. ".pdf"
            vim.cmd("!" .. "typst compile " .. vim.fn.shellescape(file) .. " " .. vim.fn.shellescape(output))
            vim.notify("Compiled to " .. output, vim.log.levels.INFO)
          end, { buffer = buf, desc = "Compile Typst to PDF" })

          -- <leader>ktd - Compile to DOCX (via Pandoc)
          vim.keymap.set("n", "<leader>ktd", function()
            local file = vim.fn.expand("%:p")
            local pdf_output = vim.fn.expand("%:p:r") .. ".pdf"
            local docx_output = vim.fn.expand("%:p:r") .. ".docx"

            -- First compile to PDF
            vim.cmd("!" .. "typst compile " .. vim.fn.shellescape(file) .. " " .. vim.fn.shellescape(pdf_output))

            -- Then convert PDF to DOCX using Pandoc
            vim.cmd(
              "!"
                .. "pandoc "
                .. vim.fn.shellescape(pdf_output)
                .. " -o "
                .. vim.fn.shellescape(docx_output)
                .. " --from=pdf --to=docx"
            )

            vim.notify("Compiled to " .. docx_output, vim.log.levels.INFO)
          end, { buffer = buf, desc = "Compile Typst to DOCX" })

          -- <leader>kto - Open compiled PDF
          vim.keymap.set("n", "<leader>kto", function()
            local pdf_file = vim.fn.expand("%:p:r") .. ".pdf"
            if vim.fn.filereadable(pdf_file) == 1 then
              if vim.fn.has("mac") == 1 then
                vim.cmd("!open " .. vim.fn.shellescape(pdf_file))
              else
                vim.cmd("!xdg-open " .. vim.fn.shellescape(pdf_file) .. " &")
              end
              vim.notify("Opened " .. pdf_file, vim.log.levels.INFO)
            else
              vim.notify("PDF file not found. Compile first with <leader>ktc", vim.log.levels.WARN)
            end
          end, { buffer = buf, desc = "Open Compiled PDF" })

          -- <leader>ktw - Watch mode (compile on save)
          vim.keymap.set("n", "<leader>ktw", function()
            local file = vim.fn.expand("%:p")
            local output = vim.fn.expand("%:p:r") .. ".pdf"
            vim.cmd("!" .. "typst watch " .. vim.fn.shellescape(file) .. " " .. vim.fn.shellescape(output) .. " &")
            vim.notify("Started watch mode for " .. file, vim.log.levels.INFO)
          end, { buffer = buf, desc = "Typst Watch Mode" })
        end,
      })

      -- Add which-key descriptions for discoverability
      local wk = require("which-key")
      wk.add({
        { "<leader>k", group = "custom" },
        { "<leader>kt", group = "typst" },
        { "<leader>ktp", desc = "Toggle Preview" },
        { "<leader>kts", desc = "Sync Preview" },
        { "<leader>ktc", desc = "Compile to PDF" },
        { "<leader>ktd", desc = "Compile to DOCX" },
        { "<leader>kto", desc = "Open PDF" },
        { "<leader>ktw", desc = "Watch Mode" },
      })
    end,
  },
}
