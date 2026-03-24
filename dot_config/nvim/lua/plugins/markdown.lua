return {
  -- Mason: Install all markdown-related LSP servers and tools
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "marksman",        -- General markdown LSP
        "markdown-oxide",  -- PKM/Obsidian LSP
        "harper-ls",       -- Lightweight grammar checking
      })
      return opts
    end,
  },

  -- LSP Configuration for Markdown
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers = opts.servers or {}

      -- Marksman: General markdown LSP (completion, navigation, broken links)
      -- Disabled in favor of markdown-oxide which provides superset of features
      -- for Obsidian vaults. Start manually with :LspStart marksman if needed.
      opts.servers.marksman = {
        enabled = true,
        autostart = false,
      }

      -- Harper: Lightweight grammar checking for markdown
      opts.servers.harper_ls = {
        enabled = true,
        autostart = true,
        filetypes = { "markdown", "text" },
        settings = {
          ["harper-ls"] = {
            linters = {
              spell_check = true,
              spelled_numbers = false,
              an_a = true,
              sentence_capitalization = true,
              unclosed_quotes = true,
              wrong_quotes = false,
              long_sentences = true,
              repeated_words = true,
              spaces = true,
              matcher = true,
            }
          }
        }
      }

      -- markdown-oxide: PKM/Obsidian features (vault completions, backlinks, tags)
      opts.servers.markdown_oxide = {
        enabled = true,
        autostart = true,
        capabilities = {
          workspace = {
            didChangeWatchedFiles = {
              dynamicRegistration = true,
            },
          },
        },
      }

      return opts
    end,
  },

  -- render-markdown.nvim: In-buffer markdown rendering
  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft = "markdown",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      file_types = { "markdown" },
      render_modes = { "n", "c" },  -- Raw in insert mode
      anti_conceal = { enabled = true },
      code = {
        enabled = true,
        sign = true,
        style = "full",
        border = "thin",
        above = "▄",
        below = "▀",
      },
      heading = {
        enabled = true,
        sign = true,
        icons = { "󰲡 ", "󰲣 ", "󰲥 ", "󰲧 ", "󰲩 ", "󰲫 " },
      },
      bullet = {
        enabled = true,
        icons = { "●", "○", "◆", "◇" },
      },
      checkbox = {
        enabled = true,
        unchecked = { icon = "󰄱 " },
        checked = { icon = "󰱒 " },
      },
      pipe_table = {
        enabled = true,
        style = "full",
        cell = "padded",
        border = {
          "┌", "┬", "┐",
          "├", "┼", "┤",
          "└", "┴", "┘",
          "│", "─",
        },
      },
      callout = {
        note = { raw = "[!NOTE]", rendered = "󰋽 Note" },
        tip = { raw = "[!TIP]", rendered = "󰌶 Tip" },
        important = { raw = "[!IMPORTANT]", rendered = "󰅾 Important" },
        warning = { raw = "[!WARNING]", rendered = "󰀪 Warning" },
        caution = { raw = "[!CAUTION]", rendered = "󰳦 Caution" },
      },
    },
  },

  -- markdown-preview.nvim: Browser-based preview with diagrams
  {
    "iamcco/markdown-preview.nvim",
    ft = "markdown",
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    config = function()
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_refresh_slow = 0
      vim.g.mkdp_command_for_global = 0
      vim.g.mkdp_preview_options = {
        mkit = {},
        katex = {},
        uml = {},
        maid = {},
        disable_sync_scroll = 0,
        sync_scroll_type = "middle",
        hide_yaml_meta = 1,
      }

      -- WSL support
      local is_wsl = vim.fn.system("grep -qi microsoft /proc/version 2>/dev/null && echo 1 || echo 0"):match("1") ~= nil
      if is_wsl then
        vim.g.mkdp_browser = "wslview"
      end
    end,
  },

  -- vim-table-mode: Easy table creation and formatting
  {
    "dhruvasagar/vim-table-mode",
    ft = "markdown",
    config = function()
      vim.g.table_mode_corner = "|"
      vim.g.table_mode_corner_corner = "|"
      vim.g.table_mode_header_fillchar = "-"
      vim.g.table_mode_map_prefix = ""
      vim.g.table_mode_syntax = 1
      vim.g.table_mode_auto_align = 1
    end,
  },

  -- img-clip.nvim: Paste images from clipboard
  {
    "HakonHarnes/img-clip.nvim",
    ft = "markdown",
    opts = {
      default = {
        dir_path = function()
          return vim.fn.expand("%:p:h") .. "/assets"
        end,
        file_name = "%Y-%m-%d-%H-%M-%S",
        template = "![$FILE_NAME]($FILE_PATH)",
        use_absolute_path = function()
          return vim.fn.expand("%:p") == ""
        end,
        prompt_for_file_name = true,
        show_dir_path_in_prompt = false,
      },
      filetypes = {
        markdown = {
          get_cmd = function()
            local is_wsl = vim.fn.system("grep -qi microsoft /proc/version 2>/dev/null && echo 1 || echo 0"):match("1") ~= nil
            if is_wsl then
              return 'powershell.exe -command "Get-Clipboard -Format Image"'
            else
              return "xclip -selection clipboard -t image/png -o"
            end
          end,
        },
      },
    },
  },

  -- markdown-toc.nvim: Table of contents generation
  {
    "hedyhli/markdown-toc.nvim",
    ft = "markdown",
    cmd = { "Mtoc" },
    opts = {
      toc_list = {
        markers = "-",
        indent_size = 2,
      },
      auto_update = true,
    },
  },

  -- Checkbox toggling (inline replacement for unmaintained markdown-togglecheck
  -- which depends on removed nvim-treesitter.ts_utils module)
  {
    "LazyVim/LazyVim",
    opts = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "markdown",
        callback = function()
          -- Toggle markdown checkbox on current line: none -> [ ] -> [x] -> [ ]
          vim.keymap.set("n", "<Plug>MarkdownToggleCheck", function()
            local line = vim.api.nvim_get_current_line()
            if line:match("%[x%]") then
              vim.api.nvim_set_current_line((line:gsub("%[x%]", "[ ]", 1)))
            elseif line:match("%[ %]") then
              vim.api.nvim_set_current_line((line:gsub("%[ %]", "[x]", 1)))
            elseif line:match("^(%s*)[%-*%+]%s") then
              vim.api.nvim_set_current_line((line:gsub("^(%s*[%-*%+])(%s)", "%1 [ ]%2", 1)))
            end
          end, { buffer = true, desc = "Toggle Checkbox" })
        end,
      })
    end,
  },

  -- Treesitter: Ensure markdown parsers installed
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "markdown",
        "markdown_inline",
      })
      return opts
    end,
  },

  -- Keybindings for Markdown
  {
    "LazyVim/LazyVim",
    opts = function()
      -- Register which-key group globally
      local wk = require("which-key")
      wk.add({
        { "<leader>km", group = "markdown", icon = "" },
      })

      -- Markdown-specific keybindings
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "markdown",
        callback = function()
          local buf = vim.api.nvim_get_current_buf()

          -- CONVERSION COMMANDS (from typst.lua) --
          -- <leader>kmc - Convert to PDF
          vim.keymap.set("n", "<leader>kmc", function()
            local file = vim.fn.expand("%:p")
            local pdf_output = vim.fn.expand("%:p:r") .. ".pdf"

            vim.notify("Converting Markdown to PDF...", vim.log.levels.INFO)
            local pandoc_cmd = string.format(
              "pandoc %s -o %s 2>&1",
              vim.fn.shellescape(file),
              vim.fn.shellescape(pdf_output)
            )
            local pandoc_output = vim.fn.system(pandoc_cmd)
            local pandoc_exit = vim.v.shell_error

            if pandoc_exit ~= 0 then
              vim.notify("Pandoc conversion failed:\n" .. pandoc_output, vim.log.levels.ERROR)
              return
            end

            if vim.fn.filereadable(pdf_output) == 1 then
              local size = vim.fn.getfsize(pdf_output)
              vim.notify(
                string.format("Successfully compiled to %s (%d bytes)", pdf_output, size),
                vim.log.levels.INFO
              )
            else
              vim.notify("PDF file was not created: " .. pdf_output, vim.log.levels.ERROR)
            end
          end, { buffer = buf, desc = "Convert to PDF" })

          -- <leader>kmd - Convert to DOCX
          vim.keymap.set("n", "<leader>kmd", function()
            local file = vim.fn.expand("%:p")
            local docx_output = vim.fn.expand("%:p:r") .. ".docx"

            vim.notify("Converting Markdown to DOCX...", vim.log.levels.INFO)
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

            if vim.fn.filereadable(docx_output) == 1 then
              local size = vim.fn.getfsize(docx_output)
              vim.notify(
                string.format("Successfully compiled to %s (%d bytes)", docx_output, size),
                vim.log.levels.INFO
              )
            else
              vim.notify("DOCX file was not created: " .. docx_output, vim.log.levels.ERROR)
            end
          end, { buffer = buf, desc = "Convert to DOCX" })

          -- PREVIEW COMMANDS --
          -- <leader>kmp - Toggle browser preview
          vim.keymap.set("n", "<leader>kmp", "<cmd>MarkdownPreviewToggle<cr>",
            { buffer = buf, desc = "Toggle Browser Preview" })

          -- <leader>kmr - Toggle render-markdown
          vim.keymap.set("n", "<leader>kmr", "<cmd>RenderMarkdown toggle<cr>",
            { buffer = buf, desc = "Toggle Render Mode" })

          -- TABLE COMMANDS --
          -- <leader>kmt - Toggle table mode
          vim.keymap.set("n", "<leader>kmt", "<cmd>TableModeToggle<cr>",
            { buffer = buf, desc = "Toggle Table Mode" })

          -- <leader>kmf - Format table
          vim.keymap.set("n", "<leader>kmf", "<cmd>TableModeRealign<cr>",
            { buffer = buf, desc = "Format Table" })

          -- IMAGE COMMANDS --
          -- <leader>kmi - Paste image from clipboard
          vim.keymap.set("n", "<leader>kmi", "<cmd>PasteImage<cr>",
            { buffer = buf, desc = "Paste Image" })

          -- CHECKBOX COMMANDS --
          -- <leader>kmx - Toggle checkbox
          vim.keymap.set("n", "<leader>kmx", "<Plug>MarkdownToggleCheck",
            { buffer = buf, desc = "Toggle Checkbox" })

          -- TOC COMMANDS --
          -- <leader>kmT - Generate/update TOC
          vim.keymap.set("n", "<leader>kmT", "<cmd>Mtoc<cr>",
            { buffer = buf, desc = "Generate TOC" })

          -- LSP COMMANDS --
          -- <leader>kmo - Start marksman LSP (general markdown, disabled by default)
          vim.keymap.set("n", "<leader>kmo", function()
            vim.cmd("LspStart marksman")
            vim.notify("Started marksman LSP", vim.log.levels.INFO)
          end, { buffer = buf, desc = "Start Marksman LSP" })

          -- Register which-key descriptions for this buffer
          wk.add({
            -- Conversions
            { "<leader>kmc", desc = "Convert to PDF", buffer = buf },
            { "<leader>kmd", desc = "Convert to DOCX", buffer = buf },

            -- Previews
            { "<leader>kmp", desc = "Toggle Browser Preview", buffer = buf },
            { "<leader>kmr", desc = "Toggle Render Mode", buffer = buf },

            -- Tables
            { "<leader>kmt", desc = "Toggle Table Mode", buffer = buf },
            { "<leader>kmf", desc = "Format Table", buffer = buf },

            -- Images
            { "<leader>kmi", desc = "Paste Image", buffer = buf },

            -- Checkboxes
            { "<leader>kmx", desc = "Toggle Checkbox", buffer = buf },

            -- TOC
            { "<leader>kmT", desc = "Generate TOC", buffer = buf },

            -- LSP
            { "<leader>kmo", desc = "Start Marksman LSP", buffer = buf },
          })
        end,
      })
    end,
  },
}
