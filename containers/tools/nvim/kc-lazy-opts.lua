-- Shared helper for the kc-nvim build scripts: resolve a plugin's merged opts
-- exactly as LazyVim would (every spec fragment's opts table/function applied).
local M = {}

function M.opts(name)
  local Config = require("lazy.core.config")
  local Plugin = require("lazy.core.plugin")
  local p = Config.plugins[name]
  if not p then
    return {}
  end
  return Plugin.values(p, "opts", false) or {}
end

function M.finish(failed, what)
  if #failed > 0 then
    io.stderr:write(("kc-nvim: %s failed: %s\n"):format(what, table.concat(failed, ", ")))
    vim.cmd("cquit 1")
  end
  vim.cmd("qall!")
end

return M
