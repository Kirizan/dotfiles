-- Typst Bibliography Management
-- Add and insert references for Typst documents

return {
  {
    "LazyVim/LazyVim",
    opts = function()
      -- Bibliography management functions
      local M = {}

      -- Entry types in priority order (online first as requested)
      M.entry_types = {
        "online",
        "article",
        "book",
        "inproceedings",
        "thesis",
        "misc",
      }

      -- Fields for each entry type
      M.fields_by_type = {
        online = { "key", "author", "title", "url", "year", "urldate", "note" },
        article = { "key", "author", "title", "journal", "year", "volume", "number", "pages", "doi" },
        book = { "key", "author", "title", "publisher", "year", "isbn", "edition" },
        inproceedings = { "key", "author", "title", "booktitle", "year", "pages", "doi" },
        thesis = { "key", "author", "title", "school", "year", "type" },
        misc = { "key", "author", "title", "howpublished", "year", "note" },
      }

      -- Field labels for display
      M.field_labels = {
        key = "Citation Key (e.g., smith2023)",
        author = "Author(s) (Last, First and Last, First)",
        title = "Title",
        url = "URL",
        year = "Year",
        urldate = "Access Date (YYYY-MM-DD)",
        note = "Note (optional)",
        journal = "Journal Name",
        volume = "Volume",
        number = "Issue Number",
        pages = "Pages (e.g., 123--456)",
        doi = "DOI",
        publisher = "Publisher",
        isbn = "ISBN",
        edition = "Edition",
        booktitle = "Book/Conference Title",
        school = "Institution/School",
        type = "Thesis Type (PhD/Masters)",
        howpublished = "How Published",
      }

      -- Find or create bibliography file
      function M.find_bib_file()
        local current_file = vim.fn.expand("%:p")
        local project_root = vim.fn.fnamemodify(current_file, ":h")

        -- Look for project root (directory with .git or first .typ file)
        while project_root ~= "/" do
          if vim.fn.isdirectory(project_root .. "/.git") == 1 then
            break
          end
          local parent = vim.fn.fnamemodify(project_root, ":h")
          if parent == project_root then
            break
          end
          project_root = parent
        end

        -- Check for existing bibliography file
        local check_paths = {
          project_root .. "/references/bibliography.bib",
          project_root .. "/bibliography/bibliography.bib",
          project_root .. "/bibliography.bib",
          project_root .. "/refs.bib",
        }

        for _, path in ipairs(check_paths) do
          if vim.fn.filereadable(path) == 1 then
            return path
          end
        end

        -- Create new file in /references/ directory (default)
        local refs_dir = project_root .. "/references"
        vim.fn.mkdir(refs_dir, "p")
        return refs_dir .. "/bibliography.bib"
      end

      -- Parse BibTeX file to extract entries
      function M.parse_bib_file(filepath)
        if vim.fn.filereadable(filepath) == 0 then
          return {}
        end

        local entries = {}
        local content = vim.fn.readfile(filepath)
        local current_entry = nil

        for _, line in ipairs(content) do
          -- Match entry start: @type{key,
          local entry_match = line:match("^%s*@(%w+)%s*{%s*([%w_%-]+)%s*,?")
          if entry_match then
            local entry_type, key = entry_match:match("(%w+).*"), entry_match:match("{([%w_%-]+)")
            current_entry = {
              type = entry_type:lower(),
              key = key,
              fields = {},
            }
            table.insert(entries, current_entry)
          elseif current_entry then
            -- Match field: field = {value},
            local field, value = line:match("%s*(%w+)%s*=%s*[{\"](.-)[}\"]%s*,?")
            if field and value then
              current_entry.fields[field:lower()] = value
            end
          end
        end

        return entries
      end

      -- Format BibTeX entry
      function M.format_bib_entry(entry_type, data)
        local lines = { string.format("@%s{%s,", entry_type, data.key) }

        for _, field in ipairs(M.fields_by_type[entry_type]) do
          if field ~= "key" and data[field] and data[field] ~= "" then
            table.insert(lines, string.format("  %s = {%s},", field, data[field]))
          end
        end

        table.insert(lines, "}")
        table.insert(lines, "")
        return table.concat(lines, "\n")
      end

      -- Write entry to bibliography file
      function M.write_bib_entry(filepath, entry)
        local file = io.open(filepath, "a")
        if file then
          file:write(entry)
          file:close()
          return true
        end
        return false
      end

      -- Add new reference (floating form)
      function M.add_reference()
        -- Step 1: Select entry type
        vim.ui.select(M.entry_types, {
          prompt = "Select reference type:",
          format_item = function(item)
            return item:gsub("^%l", string.upper)
          end,
        }, function(entry_type)
          if not entry_type then
            return
          end

          -- Step 2: Collect field data
          local data = {}
          local fields = M.fields_by_type[entry_type]

          local function collect_field(index)
            if index > #fields then
              -- All fields collected, write to file
              local bib_file = M.find_bib_file()
              local entry = M.format_bib_entry(entry_type, data)

              if M.write_bib_entry(bib_file, entry) then
                vim.notify(
                  string.format("Added reference '%s' to %s", data.key, vim.fn.fnamemodify(bib_file, ":~")),
                  vim.log.levels.INFO
                )
              else
                vim.notify("Failed to write reference", vim.log.levels.ERROR)
              end
              return
            end

            local field = fields[index]
            local label = M.field_labels[field] or field

            vim.ui.input({
              prompt = label .. ": ",
              default = "",
            }, function(value)
              if value == nil then
                -- User cancelled
                return
              end
              data[field] = value
              -- Recursively collect next field
              collect_field(index + 1)
            end)
          end

          -- Start collecting fields
          collect_field(1)
        end)
      end

      -- Insert reference at cursor using Telescope
      function M.insert_reference()
        local bib_file = M.find_bib_file()

        if vim.fn.filereadable(bib_file) == 0 then
          vim.notify("No bibliography file found. Create one first with <leader>TR", vim.log.levels.WARN)
          return
        end

        local entries = M.parse_bib_file(bib_file)

        if #entries == 0 then
          vim.notify("Bibliography file is empty. Add references with <leader>TR", vim.log.levels.WARN)
          return
        end

        -- Use Telescope picker
        local pickers = require("telescope.pickers")
        local finders = require("telescope.finders")
        local conf = require("telescope.config").values
        local actions = require("telescope.actions")
        local action_state = require("telescope.actions.state")

        pickers
          .new({}, {
            prompt_title = "Insert Reference",
            finder = finders.new_table({
              results = entries,
              entry_maker = function(entry)
                local author = entry.fields.author or "Unknown"
                local title = entry.fields.title or "No title"
                local year = entry.fields.year or "N/A"

                return {
                  value = entry,
                  display = string.format("[%s] %s - %s (%s)", entry.type, author, title, year),
                  ordinal = entry.key .. " " .. author .. " " .. title,
                  key = entry.key,
                }
              end,
            }),
            sorter = conf.generic_sorter({}),
            attach_mappings = function(prompt_bufnr, map)
              actions.select_default:replace(function()
                actions.close(prompt_bufnr)
                local selection = action_state.get_selected_entry()
                if selection then
                  -- Insert @key at cursor
                  local key = "@" .. selection.key
                  vim.api.nvim_put({ key }, "c", true, true)
                end
              end)
              return true
            end,
          })
          :find()
      end

      -- Keybindings for Typst files
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "typst",
        callback = function()
          local buf = vim.api.nvim_get_current_buf()

          -- <leader>TR - Add new reference
          vim.keymap.set("n", "<leader>TR", function()
            M.add_reference()
          end, { buffer = buf, desc = "Add Reference to Bibliography" })

          -- <leader>Tr - Insert reference at cursor
          vim.keymap.set("n", "<leader>Tr", function()
            M.insert_reference()
          end, { buffer = buf, desc = "Insert Reference Citation" })
        end,
      })

      -- Add which-key descriptions
      local wk = require("which-key")
      wk.add({
        { "<leader>TR", desc = "Add Reference" },
        { "<leader>Tr", desc = "Insert Reference" },
      })
    end,
  },
}
