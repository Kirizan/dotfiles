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
