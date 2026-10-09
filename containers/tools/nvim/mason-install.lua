-- Build-time: install every Mason package the config would auto-install —
-- mason.nvim's merged ensure_installed plus every nvim-lspconfig server that
-- Mason provides — and wait for all of them. LazyVim has no :MasonInstallAll.
-- Run via  nvim --headless "+luafile mason-install.lua"
package.path = "/opt/kc/tools/nvim/?.lua;" .. package.path
local kc = require("kc-lazy-opts")

require("lazy").load({ plugins = { "mason.nvim", "mason-lspconfig.nvim", "nvim-lspconfig" } })

local wanted = {}
for _, n in ipairs(kc.opts("mason.nvim").ensure_installed or {}) do
  wanted[n] = true
end

local okmap, mlsp = pcall(require, "mason-lspconfig")
local map = okmap and mlsp.get_mappings().lspconfig_to_package or {}
for server, sopts in pairs(kc.opts("nvim-lspconfig").servers or {}) do
  local enabled = server ~= "*" and sopts ~= false
    and (type(sopts) ~= "table" or (sopts.mason ~= false and sopts.enabled ~= false))
  if enabled and map[server] then
    wanted[map[server]] = true
  end
end

-- ltex-ls: dropped in slim builds; upstream only ships a bundled-JRE build for
-- x86_64, so it's best-effort elsewhere.
local optional = {}
if vim.env.KC_NVIM_LTEX == "false" then
  wanted["ltex-ls"] = nil
elseif vim.uv.os_uname().machine ~= "x86_64" then
  optional["ltex-ls"] = true
end

local names = vim.tbl_keys(wanted)
table.sort(names)
print("kc-nvim: mason packages: " .. table.concat(names, ", "))

local registry = require("mason-registry")
local refreshed = false
registry.refresh(function()
  refreshed = true
end)
vim.wait(5 * 60 * 1000, function()
  return refreshed
end, 200)

local failed, pending, done = {}, {}, {}
registry:on("package:install:success", function(pkg)
  done[pkg.name] = "ok"
end)
registry:on("package:install:failed", function(pkg, result)
  done[pkg.name] = "failed: " .. vim.inspect(result)
end)

for _, name in ipairs(names) do
  local okp, pkg = pcall(registry.get_package, name)
  if not okp then
    if not optional[name] then
      table.insert(failed, name .. " (not in registry)")
    end
  elseif not pkg:is_installed() then
    pending[name] = pkg
    -- LazyVim's own mason setup may already have started this one on load;
    -- either way the registry events above report the outcome.
    if not pkg:is_installing() then
      pkg:install()
    end
  end
end

vim.wait(30 * 60 * 1000, function()
  for name, pkg in pairs(pending) do
    if not done[name] and not pkg:is_installed() then
      return false
    end
  end
  return true
end, 500)

-- Quiesce: LazyVim's auto-installer can still be (re)installing something we
-- already counted. Quitting mid-install makes Mason abort AND roll the package
-- back, so wait until nothing at all is installing before the final check.
vim.wait(30 * 60 * 1000, function()
  for _, pkg in ipairs(registry.get_all_packages()) do
    if pkg:is_installing() then
      return false
    end
  end
  return true
end, 500)

for _, name in ipairs(names) do
  local okp, pkg = pcall(registry.get_package, name)
  if okp and not pending[name] and not pkg:is_installed() then
    pending[name] = pkg
  end
end

for name, pkg in pairs(pending) do
  if pkg:is_installed() then
    print("kc-nvim: installed " .. name)
  elseif optional[name] then
    print("kc-nvim: optional package unavailable on this arch: " .. name)
  else
    table.insert(failed, name .. " (" .. (done[name] or "timed out") .. ")")
  end
end
print("")
kc.finish(failed, "mason packages")
