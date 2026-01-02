# Academic Paper Templates for Typst

This directory contains reusable Typst templates for academic writing.

## Available Templates

### `academic-paper.typ`

A comprehensive template for academic papers with professional formatting, bibliography support, glossary integration, and customizable styling.

## Quick Start

### Basic Usage

```typst
#import "path/to/academic-paper.typ": academic-paper

#show: academic-paper.with(
  title: "Your Paper Title",
  author: "Your Name",
  course: "COURSE - Course Name",
)

= Introduction

Your content here...
```

### Full Example with All Features

```typst
// Import the template and utilities
#import "path/to/academic-paper.typ": academic-paper, table-style, info-box, block-quote

// Import glossarium for acronym support
#import "@preview/glossarium:0.5.9": make-glossary, print-glossary, register-glossary
#import "acronyms.typ": acronyms

// Initialize glossarium
#show: make-glossary
#register-glossary(acronyms)

// Apply the academic paper template
#show: academic-paper.with(
  // Required
  title: "D778 - Advanced Software Engineering",
  author: "Nicholas Kirby",

  // Optional metadata
  student-id: "000690574",
  course: "D778 Advanced Software Engineering",
  date: datetime.today(),

  // Document structure
  show-title-page: true,
  show-toc: true,
  toc-title: "Table of Contents",
  toc-depth: 3,

  // Font options
  font: "Liberation Sans",
  font-size: 12pt,

  // Spacing (12pt, double-spaced, 0.5in first-line indent)
  line-spacing: 2em,
  first-line-indent: 0.5in,

  // Bibliography
  bibliography-file: "bibliography/bibliography.yml",
  bibliography-style: "apa",
  bibliography-title: "References",

  // Note: Glossary is handled separately at the end
)

= Introduction

Your paper content here. You can reference @citations and @acronyms.

== Subsection

More content...

// Glossary section (must be at end, after bibliography)
#pagebreak()

= Glossary

#print-glossary(acronyms)
```

## Template Parameters

### Required Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `title` | string | Paper title |
| `author` | string | Author name |

### Optional Metadata

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `student-id` | string | none | Student ID for academic submissions |
| `course` | string | none | Course code and name |
| `date` | datetime/string | today | Document date |
| `header-text` | string/auto | auto | Custom header text (defaults to course or title) |
| `abstract` | content | none | Paper abstract |

### Font Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `font` | string | "Liberation Sans" | Body text font |
| `font-size` | length | 12pt | Body text size |
| `heading-font` | string/auto | auto | Heading font (defaults to body font) |

### Spacing Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `line-spacing` | length | 2em | Line spacing (2em = double spacing) |
| `first-line-indent` | length | 0.5in | First line indent for paragraphs |
| `paragraph-spacing` | length | 0em | Space between paragraphs |

### Page Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `paper` | string | "us-letter" | Paper size |
| `margin` | length | 1in | Page margins (all sides) |
| `page-numbering` | string | "1" | Page numbering format |

### Document Structure

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `show-title-page` | bool | true | Include title page |
| `show-toc` | bool | true | Include table of contents |
| `toc-title` | string | "Table of Contents" | TOC title |
| `toc-depth` | int | 3 | TOC depth (heading levels) |
| `toc-indent` | auto/length | auto | TOC indentation style |

### Heading Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `heading-numbering` | string/none | none | Heading numbering format (e.g., "1.1", "I.A.1") |
| `heading-numbering-levels` | int | 5 | Number of heading levels to number |

### Text Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `justify` | bool | true | Justify body text |
| `hyphenate` | bool | true | Enable hyphenation |

### Bibliography Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `bibliography-file` | string/none | none | Path to bibliography file (.yml or .bib) |
| `bibliography-style` | string | "apa" | Citation style |
| `bibliography-title` | string | "References" | Bibliography section title |

### Glossary Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `glossary-file` | string/none | none | Path to glossary/acronyms file |
| `glossary-title` | string | "Glossary" | Glossary section title |

## Utility Functions

### `table-style()`

Professional table styling for academic papers.

```typst
#show table: table-style()

#table(
  columns: 3,
  [Header 1], [Header 2], [Header 3],
  [Data 1], [Data 2], [Data 3],
  [Data 4], [Data 5], [Data 6],
)
```

Parameters:
- `header-fill`: Header background color (default: light gray)
- `stroke-width`: Border thickness (default: 0.5pt)
- `stroke-color`: Border color (default: black)
- `align-header`: Header alignment (default: center)
- `align-body`: Body alignment (default: left)

### `info-box()`

Create highlighted info/note boxes.

```typst
#info-box(
  title: "Important Note",
)[
  This is an important piece of information that needs emphasis.
]
```

Parameters:
- `content`: Box content (required)
- `title`: Box title (optional)
- `fill-color`: Background color (default: light gray)
- `radius`: Corner radius (default: 4pt)

### `block-quote()`

Format block quotations.

```typst
#block-quote(
  attribution: "Albert Einstein",
)[
  Imagination is more important than knowledge.
]
```

Parameters:
- `content`: Quote content (required)
- `attribution`: Quote source/author (optional)

### `academic-figure()`

Create properly formatted figures with captions.

```typst
#academic-figure(
  image("diagram.png"),
  caption: "System Architecture Diagram",
  label: <fig:architecture>,
)

See @fig:architecture for details.
```

Parameters:
- `content`: Figure content (required)
- `caption`: Figure caption (required)
- `label`: Cross-reference label (optional)
- `placement`: Figure placement (default: auto)

## Common Patterns

### Academic Paper with WGU Format

```typst
#import "templates/academic-paper.typ": academic-paper, info-box

#import "@preview/glossarium:0.5.9": make-glossary, print-glossary, register-glossary
#import "acronyms.typ": acronyms

#show: make-glossary
#register-glossary(acronyms)

#show: academic-paper.with(
  title: "D778 - Advanced Software Engineering",
  author: "Nicholas Kirby",
  student-id: "000690574",
  course: "D778 Advanced Software Engineering",
  font: "Liberation Sans",
  font-size: 12pt,
  line-spacing: 2em,
  first-line-indent: 0.5in,
  bibliography-file: "bibliography/bibliography.yml",
  bibliography-style: "apa",
)

= Part I: Software Development Methodology

== A. Methodology Comparison

Content here...

// References are automatically added from bibliography-file parameter

// Glossary at the end
#pagebreak()
= Glossary
#print-glossary(acronyms)
```

### Simple Paper with Different Spacing

```typst
#import "templates/academic-paper.typ": academic-paper

#show: academic-paper.with(
  title: "Research Paper",
  author: "Your Name",
  line-spacing: 1.5em,  // 1.5 spacing instead of double
  first-line-indent: 0in,  // No indent
  paragraph-spacing: 0.65em,  // Space between paragraphs
  heading-numbering: "1.1",  // Number headings
)

= Introduction

Content here...
```

### Conference Paper Format

```typst
#import "templates/academic-paper.typ": academic-paper

#show: academic-paper.with(
  title: "Paper Title",
  author: "Author Name",
  abstract: [
    This is the abstract of the paper...
  ],
  show-title-page: false,  // No separate title page
  show-toc: false,  // No table of contents
  font-size: 10pt,
  line-spacing: 1.15em,  // Single spacing
  first-line-indent: 0in,
  margin: (x: 0.75in, y: 1in),
)

= Introduction

Content...
```

## Typography Best Practices

### Fonts

Common academic fonts:
- **Liberation Sans** - Clean sans-serif (default)
- **Liberation Serif** - Traditional serif
- **Times New Roman** - Classic academic
- **Linux Libertine** - Professional serif
- **New Computer Modern** - Modern alternative to Computer Modern

### Spacing

- **Double spacing (2em)**: Standard for most academic papers
- **1.5 spacing (1.5em)**: Common for drafts and some journals
- **Single spacing (1.15em)**: Conference papers, space-constrained formats

### Margins

- **1 inch all sides**: Most common (APA, MLA)
- **1.25 inch left, 1 inch others**: Binding offset
- **0.75 inch sides, 1 inch top/bottom**: Conference papers

## Integration with typst-bib.nvim and typst-acr.nvim

This template is designed to work seamlessly with the custom Neovim plugins:

### Bibliography (typst-bib.nvim)

```typst
// Add references using the plugin: <leader>kbR
// Insert citations using the plugin: <leader>kbr

#show: academic-paper.with(
  bibliography-file: "bibliography/bibliography.yml",
  bibliography-style: "apa",
)

According to @smith2024, this approach is effective.
```

### Acronyms (typst-acr.nvim)

```typst
// Add acronyms using the plugin: <leader>kTa
// Insert acronym references using the plugin: <leader>kTi
// Cycle reference formats: <leader>kTC

#import "@preview/glossarium:0.5.9": make-glossary, print-glossary, register-glossary
#import "acronyms.typ": acronyms

#show: make-glossary
#register-glossary(acronyms)

#show: academic-paper.with(
  title: "Your Paper",
  author: "Your Name",
)

The @cpu processes data. Multiple @cpu:pl work in parallel.
First use shows @cpu:long.

// At the end
#pagebreak()
= Glossary
#print-glossary(acronyms)
```

## Tips

1. **Import order matters**: Import glossarium before applying the template
2. **Bibliography is automatic**: Set `bibliography-file` parameter, references auto-appear at end
3. **Glossary requires manual placement**: Use `#print-glossary()` where you want it
4. **Override defaults**: Any parameter can be customized per document
5. **Reusable across projects**: Import the same template for consistent formatting
6. **Version control**: Keep in dotfiles for synchronized updates across machines

## Migration from Inline Formatting

### Before (inline formatting in main.typ)

```typst
#set document(title: "Paper", author: "Name")
#set page(paper: "us-letter", margin: 1in, numbering: "1")
#set text(font: "Liberation Sans", size: 12pt)
#set par(justify: true, leading: 2em, first-line-indent: 0.5in)
#set heading(numbering: none)

#align(center)[
  #v(2in)
  #text(size: 16pt, weight: "bold")[Title]
  ...
]

= Content
...
```

### After (using template)

```typst
#import "templates/academic-paper.typ": academic-paper

#show: academic-paper.with(
  title: "Paper",
  author: "Name",
)

= Content
...
```

## License

MIT License - Free to use, modify, and distribute.

## Resources

- [Typst Documentation](https://typst.app/docs/)
- [Advanced Styling](https://typst.app/docs/tutorial/advanced-styling/)
- [Making a Template](https://typst.app/docs/tutorial/making-a-template/)
- [Bibliography Function](https://typst.app/docs/reference/model/bibliography/)
- [Glossarium Package](https://typst.app/universe/package/glossarium/)

Sources:
- [Typst Documentation - Formatting](https://typst.app/docs/tutorial/formatting/)
- [Typst Documentation - Advanced Styling](https://typst.app/docs/tutorial/advanced-styling/)
- [Typst Documentation - Making a Template](https://typst.app/docs/tutorial/making-a-template/)
- [Typst Documentation - Bibliography](https://typst.app/docs/reference/model/bibliography/)
- [Typst Universe - Templates](https://typst.app/universe/search/)
