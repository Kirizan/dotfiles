return {
  -- Project.nvim - Project detection and management
  {
    "ahmedkhalf/project.nvim",
    event = "VeryLazy",
    opts = {
      -- Detection methods (what makes a directory a "project")
      detection_methods = { "lsp", "pattern" },

      -- Patterns to detect project root
      patterns = {
        ".git",
        "_darcs",
        ".hg",
        ".bzr",
        ".svn",
        "Makefile",
        "package.json",
        "Cargo.toml",
        "go.mod",
        "pyproject.toml",
        "setup.py",
        "pom.xml",
        ".nvim.lua", -- Custom project marker
        ".project", -- Custom project marker
      },

      -- Show hidden files in Telescope (optional)
      show_hidden = false,

      -- Automatically cd to project directory
      silent_chdir = true,

      -- Scope for chdir (global, tab, win)
      scope_chdir = "global",

      -- Path to store project history
      datapath = vim.fn.stdpath("data"),
    },
    config = function(_, opts)
      require("project_nvim").setup(opts)

      -- Integrate with Telescope
      require("telescope").load_extension("projects")
    end,
  },

  -- Auto-session - Automatic session management per project
  {
    "rmagatti/auto-session",
    lazy = false,
    dependencies = {
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      log_level = "error",
      auto_session_enable_last_session = false,
      auto_session_root_dir = vim.fn.stdpath("data") .. "/sessions/",

      -- Enable auto-save and auto-restore
      auto_session_enabled = true,
      auto_save_enabled = true,
      auto_restore_enabled = true, -- Auto-restore session when opening nvim in project

      -- Suppress auto-session in certain directories
      auto_session_suppress_dirs = {
        "~/",
        "~/Downloads",
        "~/Documents",
        "~/Desktop",
        "/",
      },

      -- Use cwd for session name (makes sessions per-project)
      auto_session_use_git_branch = false,

      -- Pre/post save hooks
      pre_save_cmds = {
        "Neotree close", -- Close neo-tree before saving session
      },

      post_restore_cmds = {
        -- Optional: Reopen neo-tree after restore if you want
        -- "Neotree filesystem show",
      },

      -- Session lens configuration (Telescope picker)
      session_lens = {
        buftypes_to_ignore = {},
        load_on_setup = true,
        theme_conf = { border = true },
        previewer = false,
      },
    },
    keys = {
      { "<leader>ps", "<cmd>SessionSave<cr>", desc = "Save Session" },
      { "<leader>pr", "<cmd>SessionRestore<cr>", desc = "Restore Session" },
      { "<leader>pd", "<cmd>SessionDelete<cr>", desc = "Delete Session" },
      { "<leader>pS", "<cmd>SessionSearch<cr>", desc = "Search Sessions" },
    },
  },

  -- Template.nvim - File and project templates
  {
    "glepnir/template.nvim",
    cmd = { "Template", "TemProject" },
    opts = {
      temp_dir = vim.fn.stdpath("config") .. "/templates",
      author = "Your Name", -- Will be replaced in templates
      email = "your.email@example.com", -- Will be replaced in templates
    },
    config = function(_, opts)
      require("template").setup(opts)
    end,
  },

  -- Exrc.nvim - Secure per-project configuration
  {
    "jedrzejboczar/exrc.nvim",
    lazy = false,
    dependencies = { "MunifTanjim/nui.nvim" }, -- For trust UI
    opts = {
      files = {
        ".nvim.lua",
        ".nvimrc",
        ".exrc",
      },
    },
  },

  -- Project creation and management utilities
  {
    "LazyVim/LazyVim",
    opts = function()
      -- Project template definitions
      local templates = {
        documentation = {
          name = "Documentation Project",
          description = "Simple documentation project with README and notes",
          files = {
            ["README.md"] = [[
# {PROJECT_NAME}

## Overview
{DESCRIPTION}

## Author
{AUTHOR}

## Date
{DATE}
]],
            ["notes.md"] = [[
# Notes for {PROJECT_NAME}

## TODO
- [ ]
]],
            [".nvim.lua"] = [[
-- Project-specific Neovim settings for {PROJECT_NAME}
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = true
vim.opt_local.spelllang = "en_us"
]],
            [".project"] = "", -- Marker file
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("wincmd l")
            vim.cmd("edit README.md")
          end,
        },

        python = {
          name = "Python Project (Poetry)",
          description = "Python project with Poetry dependency management",
          files = {
            ["README.md"] = [[
# {PROJECT_NAME}

{DESCRIPTION}

## Setup
```bash
# Install Poetry if not already installed
# curl -sSL https://install.python-poetry.org | python3 -

# Install dependencies
poetry install

# Activate virtual environment
poetry shell
```

## Usage
```bash
poetry run python {PROJECT_NAME_LOWER}/main.py
```

## Development
```bash
# Add dependencies
poetry add <package>

# Add dev dependencies
poetry add --group dev <package>

# Run tests
poetry run pytest
```
]],
            ["pyproject.toml"] = [[
[tool.poetry]
name = "{PROJECT_NAME_LOWER}"
version = "0.1.0"
description = "{DESCRIPTION}"
authors = ["{AUTHOR} <{EMAIL}>"]
readme = "README.md"

[tool.poetry.dependencies]
python = "^3.11"

[tool.poetry.group.dev.dependencies]
pytest = "^7.4.0"
black = "^23.7.0"
ruff = "^0.0.285"

[build-system]
requires = ["poetry-core"]
build-backend = "poetry.core.masonry.api"
]],
            ["{PROJECT_NAME_LOWER}/__init__.py"] = "",
            ["{PROJECT_NAME_LOWER}/main.py"] = [[
#!/usr/bin/env python3
"""
{PROJECT_NAME}
{DESCRIPTION}

Author: {AUTHOR}
Date: {DATE}
"""


def main():
    """Main entry point for {PROJECT_NAME}."""
    print("Hello from {PROJECT_NAME}!")


if __name__ == "__main__":
    main()
]],
            ["tests/__init__.py"] = "",
            ["tests/test_main.py"] = [[
"""Tests for {PROJECT_NAME}."""
import pytest


def test_example():
    """Example test."""
    assert True
]],
            [".nvim.lua"] = [[
-- Python project settings
vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.expandtab = true

-- Quick commands
vim.keymap.set("n", "<leader>pr", ":!poetry run python {PROJECT_NAME_LOWER}/main.py<cr>", { buffer = true, desc = "Run main.py" })
vim.keymap.set("n", "<leader>pt", ":!poetry run pytest<cr>", { buffer = true, desc = "Run tests" })
vim.keymap.set("n", "<leader>pf", ":!poetry run black .<cr>", { buffer = true, desc = "Format code" })
]],
            [".gitignore"] = [[
# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python

# Virtual environments
.venv/
venv/
ENV/
env/

# Poetry
poetry.lock
dist/
*.egg-info/

# Testing
.pytest_cache/
.coverage
htmlcov/
.tox/

# IDE
.vscode/
.idea/
*.swp
*.swo
]],
            [".project"] = "",
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("wincmd l")
            vim.cmd("edit " .. vim.fn.expand("%:p:h") .. "/{PROJECT_NAME_LOWER}/main.py")
            vim.cmd("split")
            vim.cmd("wincmd j")
            vim.cmd("terminal")
            vim.cmd("resize 15")
            vim.cmd("wincmd k")
          end,
        },

        typescript = {
          name = "TypeScript/Node Project",
          description = "TypeScript project with package.json",
          files = {
            ["package.json"] = [[
{
  "name": "{PROJECT_NAME_LOWER}",
  "version": "0.1.0",
  "description": "{DESCRIPTION}",
  "main": "index.ts",
  "scripts": {
    "dev": "tsx watch src/index.ts",
    "build": "tsc",
    "start": "node dist/index.js"
  },
  "keywords": [],
  "author": "{AUTHOR}",
  "license": "MIT",
  "devDependencies": {
    "@types/node": "^20.0.0",
    "typescript": "^5.0.0",
    "tsx": "^4.0.0"
  }
}
]],
            ["tsconfig.json"] = [[
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "commonjs",
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
]],
            ["src/index.ts"] = [[
/**
 * {PROJECT_NAME}
 * {DESCRIPTION}
 *
 * @author {AUTHOR}
 * @date {DATE}
 */

console.log('Hello from {PROJECT_NAME}!');
]],
            ["README.md"] = [[
# {PROJECT_NAME}

{DESCRIPTION}

## Setup
```bash
npm install
```

## Development
```bash
npm run dev
```

## Build
```bash
npm run build
npm start
```
]],
            [".nvim.lua"] = [[
-- TypeScript project settings
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true

-- Quick commands
vim.keymap.set("n", "<leader>pr", ":!npm run dev<cr>", { buffer = true, desc = "Run dev server" })
vim.keymap.set("n", "<leader>pb", ":!npm run build<cr>", { buffer = true, desc = "Build project" })
]],
            [".gitignore"] = [[
node_modules/
dist/
*.log
.DS_Store
]],
            [".project"] = "",
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("wincmd l")
            vim.cmd("edit src/index.ts")
            vim.cmd("vsplit")
            vim.cmd("wincmd l")
            vim.cmd("edit package.json")
            vim.cmd("wincmd h")
          end,
        },

        notes = {
          name = "Quick Notes",
          description = "Simple note-taking project",
          files = {
            ["index.md"] = [[
# {PROJECT_NAME}

Created: {DATE}

## Notes

]],
            [".nvim.lua"] = [[
-- Note-taking settings
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = true
]],
            [".project"] = "",
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("vertical resize 25")
            vim.cmd("wincmd l")
            vim.cmd("edit index.md")
          end,
        },

        minimal = {
          name = "Minimal Project",
          description = "Bare-bones project with just README",
          files = {
            ["README.md"] = [[
# {PROJECT_NAME}

{DESCRIPTION}

Created: {DATE}
]],
            [".nvim.lua"] = [[
-- Project settings for {PROJECT_NAME}
]],
            [".project"] = "",
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("wincmd l")
            vim.cmd("edit README.md")
          end,
        },

        typst = {
          name = "Typst Academic Paper",
          description = "Typst document with academic paper template, bibliography, and acronyms",
          files = {
            ["README.md"] = [[
# {PROJECT_NAME}

{DESCRIPTION}

## Structure
- `main.typ` - Main document file
- `templates/academic-paper.typ` - Academic paper template
- `acronyms.typ` - Glossary/acronym definitions
- `bibliography/bibliography.yml` - References (Hayagriva format)

## Usage
```bash
# Preview (live reload)
typst watch main.typ

# Compile to PDF
typst compile main.typ

# Or use Neovim keybindings:
# <leader>kTp - Toggle preview
# <leader>kTc - Compile to PDF
# <leader>kTo - Open compiled PDF
```

## Adding Content
- **Citations**: Use `<leader>kTR` to add references, `<leader>kTr` to insert citations
- **Acronyms**: Use `<leader>kTa` to add acronyms, `<leader>kTi` to insert references
- **Format**: See `templates/academic-paper.typ` for all customization options
]],
            ["main.typ"] = [[
// {PROJECT_NAME}
// {DESCRIPTION}
//
// Author: {AUTHOR}
// Date: {DATE}

#import "templates/academic-paper.typ": academic-paper, glossary-table
#import "@preview/glossarium:0.5.9": make-glossary, print-glossary, register-glossary
#import "acronyms.typ": acronyms

// Initialize glossary
#show: make-glossary
#register-glossary(acronyms)

// Apply template
#show: academic-paper.with(
  title: "{PROJECT_NAME}",
  author: "{AUTHOR}",
  date: datetime.today(),

  // Optional settings (uncomment and modify as needed)
  // student-id: "000000000",
  // course: "COURSE - Course Name",
  // abstract: [Your abstract goes here...],

  // Document structure
  show-title-page: true,
  show-toc: true,
  toc-title: "Table of Contents",

  // Bibliography
  bibliography-file: "bibliography/bibliography.yml",
  bibliography-style: "apa",

  // Spacing (uncomment to customize)
  // line-spacing: 0.75em,
  // paragraph-spacing: 2em,
)

= Introduction

Write your introduction here. You can use citations like @example-citation and acronyms like @acronym-key.

= Main Content

== Section 1

Content goes here...

== Section 2

More content...

= Conclusion

Your conclusion here...

// Glossary section
#pagebreak()
= Glossary

// Display glossary as formatted list
#for entry in acronyms [
  *#entry.long* (#emph[#entry.short])#if "description" in entry and entry.description != none and entry.description != "" [: #entry.description]

]

// Render default glossary invisibly to register labels
#hide[#print-glossary(acronyms)]
]],
            ["acronyms.typ"] = [[
// Acronyms for glossarium package
// Managed by typst-acr.nvim

#let acronyms = (
  // Example acronym - delete or modify as needed
  (
    key: "example",
    short: "EX",
    long: "Example Acronym",
    description: "This is an example acronym entry",
  ),
)
]],
            ["bibliography/bibliography.yml"] = [[
# Bibliography in Hayagriva format
# Managed by typst-bib.nvim
#
# Add references using <leader>kTR or import from DOI/URL
# Insert citations using <leader>kTr

# Example reference (delete or modify as needed)
example-citation:
  type: article
  title: "Example Article Title"
  author: ["Doe, John", "Smith, Jane"]
  date: {YEAR}
  serial:
    - title: "Journal Name"
      volume: 1
      issue: 1
  page-range: 1-10
  doi: "10.1234/example"
]],
            ["templates/academic-paper.typ"] = [[
// Academic Paper Template for Typst
//
// A comprehensive template for academic papers with support for:
// - Customizable fonts, sizes, and spacing
// - Table of Contents formatting
// - Bibliography integration (Hayagriva/BibLaTeX)
// - Glossary/Acronym support via glossarium package
// - Professional table formatting
// - Heading numbering and styling
// - Title page with metadata
//
// Author: {AUTHOR}
// Date: {DATE}
// License: MIT

// ============================================
// TABLE STYLING FUNCTION
// ============================================

#let table-style(
  header-fill: rgb("#e8e8e8"),
  stroke-width: 0.5pt,
  stroke-color: black,
  align-header: center,
  align-body: left,
) = {
  it => {
    set table(
      stroke: (x, y) => (
        left: stroke-width + stroke-color,
        right: stroke-width + stroke-color,
        top: if y == 0 { stroke-width + stroke-color } else { none },
        bottom: stroke-width + stroke-color,
      ),
    )

    show table.cell.where(y: 0): set text(weight: "bold")
    show table.cell.where(y: 0): set align(align-header)
    show table.cell.where(y: 0): set fill(header-fill)

    it
  }
}

// ============================================
// MAIN TEMPLATE FUNCTION
// ============================================

#let academic-paper(
  // Required parameters
  title: none,
  author: none,

  // Optional metadata
  student-id: none,
  course: none,
  date: datetime.today(),
  header-text: auto,
  abstract: none,

  // Font options
  font: "Liberation Sans",
  font-size: 12pt,
  heading-font: auto,

  // Spacing options
  line-spacing: 0.75em,
  first-line-indent: 0in,
  paragraph-spacing: 2em,

  // Page options
  paper: "us-letter",
  margin: 1in,
  page-numbering: "1",

  // Document structure
  show-title-page: true,
  show-toc: true,
  toc-title: "Table of Contents",
  toc-depth: 3,
  toc-indent: auto,

  // Heading options
  heading-numbering: none,
  heading-numbering-levels: 5,

  // Text options
  justify: true,
  hyphenate: true,

  // Bibliography options
  bibliography-file: none,
  bibliography-style: "apa",
  bibliography-title: "References",

  // Glossary options
  glossary-file: none,
  glossary-title: "Glossary",

  // Document body
  body,
) = {

  // Validate required parameters
  assert(title != none, message: "title parameter is required")
  assert(author != none, message: "author parameter is required")

  // Set heading font to body font if not specified
  let heading-font = if heading-font == auto { font } else { heading-font }

  // Determine header text
  let header-text = if header-text == auto {
    if course != none { course } else { title }
  } else {
    header-text
  }

  // Document metadata
  set document(
    title: title,
    author: author,
    date: date,
  )

  // Page setup
  set page(
    paper: paper,
    margin: margin,
    numbering: page-numbering,
    header: if header-text != none {
      align(right)[_#header-text_]
    },
  )

  // Text and paragraph formatting
  set text(
    font: font,
    size: font-size,
    hyphenate: hyphenate,
  )

  set par(
    justify: justify,
    leading: line-spacing,
    first-line-indent: first-line-indent,
    spacing: paragraph-spacing,
  )

  // List formatting
  set list(indent: 1em, body-indent: 0.5em)
  set enum(indent: 1em, body-indent: 0.5em)

  // Heading formatting
  set heading(numbering: heading-numbering)

  show heading: it => {
    set text(font: heading-font)
    it
    v(0.5em, weak: true)
  }

  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    set text(size: 16pt, weight: "bold")
    it
    v(1em)
  }

  show heading.where(level: 2): it => {
    set text(size: 14pt, weight: "bold")
    it
    v(0.75em)
  }

  show heading.where(level: 3): it => {
    set text(size: 12pt, weight: "bold")
    it
    v(0.5em)
  }

  show heading.where(level: 4): it => {
    set text(size: 12pt, weight: "bold", style: "italic")
    it
  }

  // Bibliography setup
  if bibliography-file != none {
    set bibliography(title: bibliography-title, style: bibliography-style)
  }

  // Title page
  if show-title-page {
    align(center)[
      #v(2in)

      #text(size: 16pt, weight: "bold")[
        #title
      ]

      #v(1em)

      #author

      #if student-id != none [
        #v(0.5em)
        Student ID: #student-id
      ]

      #if course != none [
        #v(0.5em)
        #course
      ]

      #v(0.5em)

      #if type(date) == datetime [
        #date.display("[month repr:long] [day], [year]")
      ] else [
        #date
      ]

      #v(2in)
    ]

    pagebreak()
  }

  // Abstract
  if abstract != none {
    align(center)[
      #text(size: 14pt, weight: "bold")[Abstract]
    ]

    v(1em)

    set par(first-line-indent: 0in)
    abstract

    pagebreak()
  }

  // Table of contents
  if show-toc {
    outline(
      title: toc-title,
      depth: toc-depth,
      indent: toc-indent,
    )

    pagebreak()
  }

  // Main content
  body

  // Bibliography
  if bibliography-file != none {
    pagebreak()

    set par(
      hanging-indent: 0.5in,
      first-line-indent: 0in,
    )

    bibliography(bibliography-file)
  }
}

// Glossary table formatter
#let glossary-table(entries, show-group: false, show-plural: false) = {
  let cols = if show-group and show-plural {
    (auto, 1fr, 2fr, auto, auto)
  } else if show-group {
    (auto, 1fr, 2fr, auto)
  } else if show-plural {
    (auto, 1fr, 2fr, auto)
  } else {
    (auto, 1fr, 2fr)
  }

  let header = if show-group and show-plural {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Plural*], [*Group*])
  } else if show-group {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Group*])
  } else if show-plural {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Plural*])
  } else {
    ([*Abbreviation*], [*Long Form*], [*Description*])
  }

  table(
    columns: cols,
    stroke: 0.5pt,
    fill: (x, y) => if y == 0 { rgb("#e8e8e8") },
    align: (x, y) => if y == 0 { center } else { left },
    inset: 8pt,

    ..header,

    ..entries.map(entry => {
      let row = (
        entry.short,
        entry.long,
        entry.at("description", default: ""),
      )

      if show-plural {
        row.push(entry.at("plural", default: ""))
      }

      if show-group {
        row.push(entry.at("group", default: ""))
      }

      row
    }).flatten()
  )
}
]],
            [".nvim.lua"] = [[
-- Typst project settings for {PROJECT_NAME}
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = true
vim.opt_local.spelllang = "en_us"

-- Project-specific keybindings
vim.keymap.set("n", "<leader>pb", ":!typst compile main.typ<cr>", { buffer = true, desc = "Build PDF" })
vim.keymap.set("n", "<leader>pw", ":!typst watch main.typ &<cr>", { buffer = true, desc = "Watch mode" })
]],
            [".gitignore"] = [[
# Typst output
*.pdf

# Editor
*.swp
*.swo
.DS_Store
]],
            [".typst-acr.toml"] = [[
# Typst-acr configuration for {PROJECT_NAME}
acr_file = "acronyms.typ"
glossarium_var_name = "acronyms"
]],
            [".project"] = "",
          },
          layout = function()
            vim.cmd("Neotree filesystem reveal left")
            vim.cmd("vertical resize 30")
            vim.cmd("wincmd l")
            vim.cmd("edit main.typ")
            vim.cmd("TypstPreview")
          end,
        },
      }

      -- Function to create a project from template
      local function create_project()
        -- Get template choices
        local template_choices = {}
        for key, tmpl in pairs(templates) do
          table.insert(template_choices, string.format("%s - %s", tmpl.name, tmpl.description))
        end

        -- Select template
        vim.ui.select(template_choices, {
          prompt = "Select project template:",
        }, function(choice)
          if not choice then
            return
          end

          -- Find selected template
          local selected_template = nil
          local selected_key = nil
          for key, tmpl in pairs(templates) do
            if choice:match("^" .. tmpl.name) then
              selected_template = tmpl
              selected_key = key
              break
            end
          end

          if not selected_template then
            return
          end

          -- Get project name
          vim.ui.input({
            prompt = "Project name: ",
          }, function(project_name)
            if not project_name or project_name == "" then
              return
            end

            -- Get project description
            vim.ui.input({
              prompt = "Description: ",
            }, function(description)
              if not description then
                description = ""
              end

              -- Get base directory
              vim.ui.input({
                prompt = "Base directory (default: ~/projects): ",
                default = vim.fn.expand("~/projects"),
              }, function(base_dir)
                if not base_dir or base_dir == "" then
                  base_dir = vim.fn.expand("~/projects")
                end

                -- Expand ~ in path
                base_dir = vim.fn.expand(base_dir)

                -- Create project directory
                local project_dir = base_dir .. "/" .. project_name
                if vim.fn.isdirectory(project_dir) == 1 then
                  vim.notify("Project directory already exists: " .. project_dir, vim.log.levels.ERROR)
                  return
                end

                -- Create directory
                vim.fn.mkdir(project_dir, "p")

                -- Template variables
                local vars = {
                  PROJECT_NAME = project_name,
                  PROJECT_NAME_LOWER = project_name:lower():gsub("%s+", "-"),
                  DESCRIPTION = description,
                  AUTHOR = vim.fn.system("git config user.name"):gsub("\n", ""),
                  EMAIL = vim.fn.system("git config user.email"):gsub("\n", ""),
                  DATE = os.date("%Y-%m-%d"),
                  YEAR = os.date("%Y"),
                }

                -- Create files from template
                for filename, content in pairs(selected_template.files) do
                  local filepath = project_dir .. "/" .. filename

                  -- Create parent directories if needed
                  local parent = vim.fn.fnamemodify(filepath, ":h")
                  if vim.fn.isdirectory(parent) == 0 then
                    vim.fn.mkdir(parent, "p")
                  end

                  -- Replace template variables
                  for var, value in pairs(vars) do
                    content = content:gsub("{" .. var .. "}", value)
                  end

                  -- Write file
                  local file = io.open(filepath, "w")
                  if file then
                    file:write(content)
                    file:close()
                  end
                end

                -- Initialize git repository
                vim.fn.system("git init " .. project_dir)
                vim.fn.system("cd " .. project_dir .. " && git add . && git commit -m 'Initial commit'")

                -- Change to project directory
                vim.cmd("cd " .. project_dir)

                -- Apply project layout if defined
                if selected_template.layout then
                  vim.schedule(function()
                    selected_template.layout()
                  end)
                end

                vim.notify(
                  "✓ Created project: " .. project_name .. "\n  Location: " .. project_dir,
                  vim.log.levels.INFO
                )
              end)
            end)
          end)
        end)
      end

      -- Keybindings
      vim.keymap.set("n", "<leader>pn", create_project, { desc = "New Project from Template" })
      vim.keymap.set("n", "<leader>pp", "<cmd>Telescope projects<cr>", { desc = "Switch Project" })
      vim.keymap.set("n", "<leader>pf", "<cmd>Telescope find_files<cr>", { desc = "Find Files in Project" })
      vim.keymap.set("n", "<leader>pg", "<cmd>Telescope live_grep<cr>", { desc = "Grep in Project" })
    end,
  },

  -- Update Telescope configuration
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      { "<leader>pp", "<cmd>Telescope projects<cr>", desc = "Projects" },
    },
  },

  -- Configure which-key for project management
  {
    "folke/which-key.nvim",
    optional = true,
    opts = {
      spec = {
        { "<leader>p", group = "projects", icon = " " },
      },
    },
  },
}
