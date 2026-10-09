-- Every plugin Lazy considers enabled in this image must already be installed.
-- (Plugins with a false `cond`, such as private repos, are excluded by Lazy.)
local missing, n = {}, 0
for _, p in ipairs(require("lazy").plugins()) do
  n = n + 1
  if not (p._ and p._.installed) then
    table.insert(missing, p.name)
  end
end
print(("kc-nvim smoke: %d enabled plugins, %d missing"):format(n, #missing))
if #missing > 0 then
  io.stderr:write("kc-nvim smoke: not installed: " .. table.concat(missing, ", ") .. "\n")
  vim.cmd("cquit 1")
end
vim.cmd("qall!")
