-- Clipboard inside kc container images (containers/ in the dotfiles repo).
--
-- There's no display server, wl-copy, pbcopy or clip.exe in the container, so
-- use OSC 52: the terminal itself sets the system clipboard. Works in WezTerm,
-- over SSH, and through tmux (set-clipboard on). Paste via the terminal's own
-- paste always works; OSC 52 *read* support varies by terminal.
if not vim.env.KC_IN_CONTAINER then
  return {}
end

return {
  {
    "LazyVim/LazyVim",
    opts = function()
      vim.g.clipboard = "osc52"
      vim.opt.clipboard = "unnamedplus"
    end,
  },
}
