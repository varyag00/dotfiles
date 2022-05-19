--[[
lvim is the global options object

LRinters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT

-- general
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.colorscheme = "shades_of_purple"
-- lvim.use_icons = false

vim.opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard TODO doesn't work
vim.opt.guifont = "JetBrainsMonoNL NF:h11" -- the font used in graphical neovim applications

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"
-- unmap a default keymapping
-- lvim.keys.normal_mode["<C-Up>"] = false
-- edit a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>"

-- navigate between buffers with alt
lvim.keys.normal_mode["<A-h>"] = ":bprev<cr>"
lvim.keys.normal_mode["<A-l>"] = ":bnext<cr>"
lvim.keys.normal_mode["<A-1>"] = ":buffer 1<cr>"
lvim.keys.normal_mode["<A-2>"] = ":buffer 2<cr>"
lvim.keys.normal_mode["<A-3>"] = ":buffer 3<cr>"
lvim.keys.normal_mode["<A-4>"] = ":buffer 4<cr>"
lvim.keys.normal_mode["<A-5>"] = ":buffer 5<cr>"


-- TODO: doesn't work - maybe C-/ needs escaping or something?
-- lvim.keys.normal_mode["<C-/>"] = "<Plug>comment_toggle_linewise"

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- we use protected-mode (pcall) just in case the plugin wasn't loaded yet.
local _, actions = pcall(require, "telescope.actions")
lvim.builtin.telescope.defaults.mappings = {
  -- for input mode
  i = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
    ["<C-n>"] = actions.cycle_history_next,
    ["<C-p>"] = actions.cycle_history_prev,
  },
  -- for normal mode
  n = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
  },
}

-- whichkey --

-- Use which-key to add extra bindings with the leader-key prefix
lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }
lvim.builtin.which_key.mappings["t"] = {
  name = "+Trouble",
  r = { "<cmd>Trouble lsp_references<cr>", "References" },
  f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
  d = { "<cmd>Trouble document_diagnostics<cr>", "Diagnostics" },
  q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
  l = { "<cmd>Trouble loclist<cr>", "LocationList" },
  w = { "<cmd>Trouble workspace_diagnostics<cr>", "Wordspace Diagnostics" },
}

-- TODO: add "open" submenu
-- lvim.builtin.which_key.mappings["o"] = {
--   name = "+Open",
--   s = { "<c-w>s<cr>", "Split horizontal" },


-- overwrite default leader-w (i.e. save) to window
lvim.builtin.which_key.mappings["w"] = {
  name = "+Window",
  s = { "<c-w>s<cr>", "Split horizontal" },
  v = { "<c-w>v<cr>", "Split vertical" },
  c = { "<c-w>c<cr>", "Close window" },
  L = { "<c-w>L<cr>", "Move window right" },
  H = { "<c-w>H<cr>", "Move window left" },
  J = { "<c-w>J<cr>", "Move window down" },
  K = { "<c-w>K<cr>", "Move window up" },
}

-- replace existing submenu
lvim.builtin.which_key.mappings["bn"] = {
  ":bnext<cr>", "next"
}
lvim.builtin.which_key.mappings["bp"] = {
  ":bprev<cr>", "previous"
}
lvim.builtin.which_key.mappings["bk"] = {
  ":bufferkill<cr>", "kill"
}
lvim.builtin.which_key.mappings["bd"] = {
  ":bufferkill<cr>", "kill"
}
-- TODO delete current buffer
-- TODO install comment-noting plugins from doom-nvim
-- illuminated
-- kommentary (already installed?)
-- todo_comments

-- TODO works, but it would be nicer inside of "g", unfortunately it somehow breaks lvim
lvim.builtin.which_key.mappings["G"] = {
  name = "+Magit",
  s = { ":Neogit<cr>", "status" }
}


-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.notify.active = true
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.show_icons.git = 1

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "tsx",
  "css",
  "rust",
  "java",
  "yaml",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true

-- generic LSP settings

-- ---@usage disable automatic installation of servers
-- lvim.lsp.automatic_servers_installation = false

-- ---configure a server manually. !!Requires `:LvimCacheReset` to take effect!!
-- ---see the full default list `:lua print(vim.inspect(lvim.lsp.automatic_configuration.skipped_servers))`
-- vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "pyright" })
-- local opts = {} -- check the lspconfig documentation for a list of all possible options
-- require("lvim.lsp.manager").setup("pyright", opts)

-- ---remove a server from the skipped list, e.g. eslint, or emmet_ls. !!Requires `:LvimCacheReset` to take effect!!
-- ---`:LvimInfo` lists which server(s) are skiipped for the current filetype
-- vim.tbl_map(function(server)
--   return server ~= "emmet_ls"
-- end, lvim.lsp.automatic_configuration.skipped_servers)

-- -- you can set a custom on_attach function that will be used for all the language servers
-- -- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end

-- -- set a formatter, this will override the language server formatting capabilities (if it exists)
-- local formatters = require "lvim.lsp.null-ls.formatters"
-- formatters.setup {
--   { command = "black", filetypes = { "python" } },
--   { command = "isort", filetypes = { "python" } },
--   {
--     -- each formatter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
--     command = "prettier",
--     ---@usage arguments to pass to the formatter
--     -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
--     extra_args = { "--print-with", "100" },
--     ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
--     filetypes = { "typescript", "typescriptreact" },
--   },
-- }

-- -- set additional linters
local linters = require "lvim.lsp.null-ls.linters"
linters.setup {
  { command = "pylint", filetypes = { "python" } },
  { command = "flake8", filetypes = { "python" } }, -- original command
  --   {
  --     -- each linter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
  --     command = "shellcheck",
  --     ---@usage arguments to pass to the formatter
  --     -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
  --     extra_args = { "--severity", "warning" },
  --   },
  {
    command = "codespell",
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    filetypes = { "javascript", "python" },
  },
}

-- Additional Plugins
lvim.plugins = {
  { "folke/tokyonight.nvim" },
  {
    "folke/trouble.nvim",
    cmd = "TroubleToggle",
  },
  { 'Rigellute/shades-of-purple.vim' },
  { 'LunarVim/darkplus.nvim' },
  { 'LunarVim/onedarker.nvim' },
  { 'TimUntersberger/neogit' },
  { 'folke/todo-comments.nvim' },
  -- highlight ranges like :10-15
  { 'winston0410/range-highlight.nvim' },
  -- run `:Copilot setup` afterwards
  -- { "github/copilot.vim" }, -- has weird behaviour
  { "gelfand/copilot.vim" },

}
require("todo-comments").setup {
  -- NOTE: see https://github.com/folke/todo-comments.nvim
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section below
}

-- TODO: Improvements
-- unbind remove moving line with <A-j/k> - see https://github.com/LunarVim/LunarVim/blob/4400e39a69dce6c2a63b391242e38f781e35025d/lua/lvim/keymappings.lua#L3

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- lvim.autocommands.custom_groups = {
--   { "BufWinEnter", "*.lua", "setlocal ts=8 sw=8" },
-- }
