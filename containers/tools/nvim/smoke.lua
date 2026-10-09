-- kc-nvim LSP smoke check. Invoked by smoke.sh as
--   KC_SMOKE_CLIENTS="a b" nvim --headless --cmd "luafile smoke.lua" <file>
-- Registered via --cmd so it runs after VimEnter: vim.lsp.enable() only attaches
-- to already-open buffers once v:vim_did_enter is set, and `+cmd` arguments run
-- before that — which made headless checks report zero clients even on a host.
local expected = vim.split(vim.env.KC_SMOKE_CLIENTS or "", " ", { trimempty = true })

vim.api.nvim_create_autocmd("VimEnter", {
  once = true,
  callback = function()
    vim.schedule(function()
      local buf = vim.api.nvim_get_current_buf()
      local function attached()
        local names = {}
        for _, c in ipairs(vim.lsp.get_clients({ bufnr = buf })) do
          names[c.name] = true
        end
        return names
      end
      vim.wait(90000, function()
        local names = attached()
        for _, e in ipairs(expected) do
          if not names[e] then
            return false
          end
        end
        return true
      end, 250)

      local names, missing = attached(), {}
      for _, e in ipairs(expected) do
        if not names[e] then
          table.insert(missing, e)
        end
      end
      local got = vim.tbl_keys(names)
      table.sort(got)
      io.stdout:write(("kc-nvim smoke: %s (ft=%s) attached: %s\n"):format(
        vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t"), vim.bo[buf].filetype, table.concat(got, ", ")))
      if #missing > 0 then
        io.stdout:write("kc-nvim smoke: missing LSP clients: " .. table.concat(missing, ", ") .. "\n")
        vim.cmd("cquit 1")
      end
      vim.cmd("qall!")
    end)
  end,
})
