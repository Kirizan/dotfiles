return {
  -- Typst language support with LSP (tinymist)
  {
    "chomosuke/typst-preview.nvim",
    lazy = false, -- Load immediately for .typ files
    version = "1.*",  -- Use latest 1.x version (supports newer typst)
    build = function()
      require("typst-preview").update()
    end,
    ft = "typst",
    opts = function()
      -- Detect WSL or Docker environment
      local is_wsl = vim.fn.system("grep -qi microsoft /proc/version 2>/dev/null && echo 1 || echo 0"):match("1") ~= nil
      local is_docker = vim.fn.filereadable("/.dockerenv") == 1

      local config = {
        dependencies_bin = {
          ["typst-preview"] = nil, -- Will use bundled version
          ["websocat"] = nil, -- Will use bundled version
        },
      }

      -- Configure for WSL/Docker environments
      if is_wsl or is_docker then
        -- Get WSL host IP for accessing from Windows
        local host_ip = "localhost"
        if is_wsl then
          local wsl_host = vim.fn.system("ip route show | grep -i default | awk '{ print $3}'"):gsub("%s+", "")
          if wsl_host ~= "" then
            host_ip = wsl_host
          end
        end

        config.get_browser_cmd = function(url)
          if is_wsl then
            -- Open in Windows browser from WSL
            -- Try wslview first (if wslu is installed), fallback to explorer.exe
            if vim.fn.executable("wslview") == 1 then
              return { "wslview", url }
            else
              -- explorer.exe can open URLs
              return { "explorer.exe", url }
            end
          elseif is_docker then
            -- For Docker, print URL for manual access
            -- Replace localhost with host IP if available
            local external_url = url:gsub("localhost", host_ip)
            vim.notify(
              "Docker detected - Open in host browser:\n" .. external_url,
              vim.log.levels.INFO,
              { timeout = 10000 }
            )
            return nil -- Don't try to auto-open
          end
        end

        -- Bind to 0.0.0.0 to allow external access from WSL/Docker
        config.server_opts = {
          host = "0.0.0.0",
        }
      end

      return config
    end,
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
    "mason-org/mason.nvim",
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
      -- Register which-key groups globally (before autocmd)
      local wk = require("which-key")
      wk.add({
        { "<leader>k", group = "KirDoIt" },
        { "<leader>kT", group = "typst", icon = "📄" },
        -- Visual mode groups
        { "<leader>k", group = "KirDoIt", mode = "v" },
        { "<leader>kT", group = "typst", icon = "📄", mode = "v" },
      })

      -- Typst-specific keybindings under <leader>kT (uppercase T)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "typst",
        callback = function()
          local buf = vim.api.nvim_get_current_buf()

          -- <leader>kTp - Toggle preview
          vim.keymap.set("n", "<leader>kTp", "<cmd>TypstPreviewToggle<cr>", { buffer = buf, desc = "Toggle Typst Preview" })

          -- <leader>kTs - Sync preview (jump to current position)
          vim.keymap.set("n", "<leader>kTs", "<cmd>TypstPreviewSyncCursor<cr>", { buffer = buf, desc = "Sync Typst Preview" })

          -- <leader>kTc - Compile to PDF
          vim.keymap.set("n", "<leader>kTc", function()
            local file = vim.fn.expand("%:p")
            local output = vim.fn.expand("%:p:r") .. ".pdf"
            vim.cmd("!" .. "typst compile " .. vim.fn.shellescape(file) .. " " .. vim.fn.shellescape(output))
            vim.notify("Compiled to " .. output, vim.log.levels.INFO)
          end, { buffer = buf, desc = "Compile Typst to PDF" })

          -- <leader>kTd - Compile to DOCX (via Pandoc)
          vim.keymap.set("n", "<leader>kTd", function()
            local file = vim.fn.expand("%:p")
            local docx_output = vim.fn.expand("%:p:r") .. ".docx"

            -- Convert Typst directly to DOCX using Pandoc
            -- Note: Pandoc has limited support for Typst, so formatting may be basic
            vim.notify("Converting Typst to DOCX...", vim.log.levels.INFO)
            local pandoc_cmd = string.format(
              "pandoc %s -o %s 2>&1",
              vim.fn.shellescape(file),
              vim.fn.shellescape(docx_output)
            )
            local pandoc_output = vim.fn.system(pandoc_cmd)
            local pandoc_exit = vim.v.shell_error

            if pandoc_exit ~= 0 then
              vim.notify("Pandoc conversion failed:\n" .. pandoc_output, vim.log.levels.ERROR)
              return
            end

            -- Check if DOCX was created
            if vim.fn.filereadable(docx_output) == 1 then
              local size = vim.fn.getfsize(docx_output)
              vim.notify(
                string.format("Successfully compiled to %s (%d bytes)", docx_output, size),
                vim.log.levels.INFO
              )
            else
              vim.notify("DOCX file was not created: " .. docx_output, vim.log.levels.ERROR)
            end
          end, { buffer = buf, desc = "Compile Typst to DOCX" })

          -- <leader>kTo - Open compiled PDF
          vim.keymap.set("n", "<leader>kTo", function()
            local pdf_file = vim.fn.expand("%:p:r") .. ".pdf"
            if vim.fn.filereadable(pdf_file) == 1 then
              if vim.fn.has("mac") == 1 then
                vim.cmd("!open " .. vim.fn.shellescape(pdf_file))
              else
                vim.cmd("!xdg-open " .. vim.fn.shellescape(pdf_file) .. " &")
              end
              vim.notify("Opened " .. pdf_file, vim.log.levels.INFO)
            else
              vim.notify("PDF file not found. Compile first with <leader>kTc", vim.log.levels.WARN)
            end
          end, { buffer = buf, desc = "Open Compiled PDF" })

          -- <leader>kTw - Watch mode (compile on save)
          vim.keymap.set("n", "<leader>kTw", function()
            local file = vim.fn.expand("%:p")
            local output = vim.fn.expand("%:p:r") .. ".pdf"
            vim.cmd("!" .. "typst watch " .. vim.fn.shellescape(file) .. " " .. vim.fn.shellescape(output) .. " &")
            vim.notify("Started watch mode for " .. file, vim.log.levels.INFO)
          end, { buffer = buf, desc = "Typst Watch Mode" })

          -- <leader>kTR - Add reference to bibliography
          vim.keymap.set("n", "<leader>kTR", "<cmd>TypstBibAdd<cr>", { buffer = buf, desc = "Add Reference" })

          -- <leader>kTr - Insert citation
          vim.keymap.set("n", "<leader>kTr", "<cmd>TypstBibInsert<cr>", { buffer = buf, desc = "Insert Citation" })

          -- <leader>kTI - Import reference from DOI
          vim.keymap.set("n", "<leader>kTI", "<cmd>TypstBibImportDOI<cr>", { buffer = buf, desc = "Import from DOI" })

          -- <leader>kTU - Import reference from URL
          vim.keymap.set("n", "<leader>kTU", "<cmd>TypstBibImportURL<cr>", { buffer = buf, desc = "Import from URL" })

          -- <leader>kT - Acronym management
          -- <leader>kTa - Add acronym
          vim.keymap.set("n", "<leader>kTa", "<cmd>TypstAcrAdd<cr>", { buffer = buf, desc = "Add Acronym" })
          -- Visual mode: capture selection before calling command
          vim.keymap.set("v", "<leader>kTa", function()
            require("typst-acr").add_acronym_from_selection()
          end, { buffer = buf, desc = "Add Acronym from Selection" })

          -- <leader>kTi - Insert acronym reference
          vim.keymap.set("n", "<leader>kTi", "<cmd>TypstAcrInsert<cr>", { buffer = buf, desc = "Insert Acronym Reference" })

          -- <leader>kTC - Cycle reference format
          vim.keymap.set("n", "<leader>kTC", "<cmd>TypstAcrCycle<cr>", { buffer = buf, desc = "Cycle Acronym Format" })

          -- <leader>kTl - List all acronyms
          vim.keymap.set("n", "<leader>kTl", "<cmd>TypstAcrList<cr>", { buffer = buf, desc = "List Acronyms" })

          -- <leader>kTv - Validate acronym file
          vim.keymap.set("n", "<leader>kTv", "<cmd>TypstAcrValidate<cr>", { buffer = buf, desc = "Validate Acronyms" })

          -- <leader>kTe - Edit config (edit .typst-acr.toml)
          vim.keymap.set("n", "<leader>kTe", "<cmd>TypstAcrConfigure<cr>", { buffer = buf, desc = "Edit Acronym Config" })

          -- Register which-key descriptions for this buffer
          wk.add({
            -- Document operations
            { "<leader>kTp", desc = "Toggle Preview", buffer = buf },
            { "<leader>kTs", desc = "Sync Preview", buffer = buf },
            { "<leader>kTc", desc = "Compile to PDF", buffer = buf },
            { "<leader>kTd", desc = "Compile to DOCX", buffer = buf },
            { "<leader>kTo", desc = "Open PDF", buffer = buf },
            { "<leader>kTw", desc = "Watch Mode", buffer = buf },
            -- Bibliography
            { "<leader>kTR", desc = "Add Reference", buffer = buf },
            { "<leader>kTr", desc = "Insert Citation", buffer = buf },
            { "<leader>kTI", desc = "Import from DOI", buffer = buf },
            { "<leader>kTU", desc = "Import from URL", buffer = buf },
            -- Acronyms
            { "<leader>kTa", desc = "Add Acronym", buffer = buf },
            { "<leader>kTi", desc = "Insert Acronym", buffer = buf },
            { "<leader>kTC", desc = "Cycle Acronym", buffer = buf },
            { "<leader>kTl", desc = "List Acronyms", buffer = buf },
            { "<leader>kTv", desc = "Validate Acronyms", buffer = buf },
            { "<leader>kTe", desc = "Edit Acronym Config", buffer = buf },
            -- Visual mode mappings
            { "<leader>kTa", desc = "Add Acronym from Selection", mode = "v", buffer = buf },
          })
        end,
      })
    end,
  },
}
