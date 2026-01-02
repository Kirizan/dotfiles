return {
  -- spellwarn.nvim - Display spelling errors as LSP-style diagnostics
  {
    "ravibrock/spellwarn.nvim",
    event = "VeryLazy",
    config = function()
      require("spellwarn").setup({
        -- Enable for these filetypes
        enable_ft = { "typst", "markdown", "text", "gitcommit", "tex" },
        -- Prefix for diagnostic messages
        prefix = "Spelling: ",
        -- Diagnostic severity (Error, Warn, Info, Hint)
        severity = {
          spellbad = "Warn",   -- Misspelled words
          spellcap = "Hint",   -- Capitalization suggestions
          spellrare = "Info",  -- Rare words
          spelllocal = "Info", -- Wrong spelling for selected region
        },
      })
    end,
  },

  -- cmp-spell - Completion source for spell checking
  {
    "f3fora/cmp-spell",
    dependencies = { "hrsh7th/nvim-cmp" },
    ft = { "typst", "markdown", "text", "gitcommit", "tex" },
  },

  -- Configure nvim-cmp to use spell source
  {
    "hrsh7th/nvim-cmp",
    optional = true,
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources or {}, {
        {
          name = "spell",
          option = {
            keep_all_entries = false,
            enable_in_context = function()
              return true
            end,
          },
        },
      }))
    end,
  },

  -- Configure Neovim's built-in spell checker
  {
    "LazyVim/LazyVim",
    opts = function()
      -- Enable spell checking for specific filetypes
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "typst", "markdown", "text", "gitcommit", "tex" },
        callback = function()
          vim.opt_local.spell = true
          vim.opt_local.spelllang = "en_us"
        end,
      })

      -- Spell checking keybindings
      vim.keymap.set("n", "<leader>us", function()
        vim.opt.spell = not vim.opt.spell:get()
        if vim.opt.spell:get() then
          vim.notify("Spell checking enabled", vim.log.levels.INFO)
        else
          vim.notify("Spell checking disabled", vim.log.levels.INFO)
        end
      end, { desc = "Toggle Spell Check" })

      -- <leader>sc - Show spelling suggestions (using Telescope or vim.ui.select)
      vim.keymap.set("n", "<leader>sc", function()
        local word = vim.fn.expand("<cword>")
        local suggestions = vim.fn.spellsuggest(word, 10)

        if #suggestions == 0 then
          vim.notify("No spelling suggestions for: " .. word, vim.log.levels.WARN)
          return
        end

        -- Use Telescope if available, otherwise vim.ui.select
        local ok, telescope = pcall(require, "telescope.builtin")
        if ok then
          require("telescope.pickers")
            .new({}, {
              prompt_title = "Spelling Suggestions for: " .. word,
              finder = require("telescope.finders").new_table({
                results = suggestions,
              }),
              sorter = require("telescope.config").values.generic_sorter({}),
              attach_mappings = function(prompt_bufnr)
                require("telescope.actions").select_default:replace(function()
                  local selection = require("telescope.actions.state").get_selected_entry()
                  require("telescope.actions").close(prompt_bufnr)
                  vim.cmd("normal! ciw" .. selection[1])
                end)
                return true
              end,
            })
            :find()
        else
          -- Fallback to vim.ui.select
          vim.ui.select(suggestions, {
            prompt = "Spelling suggestions for: " .. word,
          }, function(choice)
            if choice then
              vim.cmd("normal! ciw" .. choice)
            end
          end)
        end
      end, { desc = "Spelling Corrections" })

      -- <leader>si - Add word to dictionary (ignore permanently)
      vim.keymap.set("n", "<leader>si", function()
        local word = vim.fn.expand("<cword>")
        vim.cmd("spellgood! " .. word)
        vim.notify("Added '" .. word .. "' to dictionary", vim.log.levels.INFO)
      end, { desc = "Add to Dictionary" })

      -- <leader>sI - Ignore word in current session only
      vim.keymap.set("n", "<leader>sI", function()
        local word = vim.fn.expand("<cword>")
        vim.cmd("spellgood " .. word) -- Without !, only for current session
        vim.notify("Ignoring '" .. word .. "' in current session", vim.log.levels.INFO)
      end, { desc = "Ignore (Session Only)" })

      -- z= alternative - Quick fix (keep vim default but also add to which-key)
      vim.keymap.set("n", "z=", "z=", { desc = "Spelling Suggestions (vim)" })
    end,
  },

  -- Configure which-key for spell checking group
  {
    "folke/which-key.nvim",
    optional = true,
    opts = {
      spec = {
        { "<leader>s", group = "spell" },
      },
    },
  },
}
