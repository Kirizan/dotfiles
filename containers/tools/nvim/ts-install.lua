-- Build-time: compile every treesitter parser the config asks for, so the
-- image never needs a compiler or network at runtime. Run via
--   nvim --headless "+luafile ts-install.lua"
-- (nvim-treesitter `main` branch: :TSUpdateSync no longer exists.)
package.path = "/opt/kc/tools/nvim/?.lua;" .. package.path
local kc = require("kc-lazy-opts")

require("lazy").load({ plugins = { "nvim-treesitter" } })
local langs = kc.opts("nvim-treesitter").ensure_installed or {}
print(("kc-nvim: installing %d treesitter parsers"):format(#langs))

local ok, err = pcall(function()
  require("nvim-treesitter").install(langs, { summary = true }):wait(30 * 60 * 1000)
end)

local failed = {}
if not ok then
  table.insert(failed, tostring(err))
end
local installed = {}
for _, l in ipairs(require("nvim-treesitter").get_installed("parsers")) do
  installed[l] = true
end
-- LazyVim's list can name languages the nvim-treesitter `main` registry no longer
-- ships as separate parsers (e.g. jsonc). Those can't be installed by anyone, so
-- report them rather than failing the build.
local registry = require("nvim-treesitter.parsers")
for _, l in ipairs(langs) do
  if not installed[l] then
    if registry[l] == nil then
      print("kc-nvim: not in the nvim-treesitter registry, skipped: " .. l)
    else
      table.insert(failed, l)
    end
  end
end
print(("\nkc-nvim: %d parsers installed"):format(vim.tbl_count(installed)))
kc.finish(failed, "treesitter parsers")
