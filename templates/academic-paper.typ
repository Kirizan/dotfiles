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
// Usage:
//   #import "path/to/academic-paper.typ": academic-paper, table-style
//
//   #show: academic-paper.with(
//     title: "Your Paper Title",
//     author: "Your Name",
//     student-id: "000000000",  // Optional
//     course: "COURSE - Course Name",  // Optional
//     date: datetime.today(),
//   )
//
// Author: Nicholas Kirby
// License: MIT

// ============================================
// TABLE STYLING FUNCTION
// ============================================

/// Professional table styling for academic papers
///
/// Parameters:
///   - header-fill: Background color for header row (default: light gray)
///   - stroke-width: Border thickness (default: 0.5pt)
///   - stroke-color: Border color (default: black)
///   - align-header: Header text alignment (default: center)
///   - align-body: Body text alignment (default: left)
#let table-style(
  header-fill: rgb("#e8e8e8"),
  stroke-width: 0.5pt,
  stroke-color: black,
  align-header: center,
  align-body: left,
) = {
  // Return a function that can be used with show rules
  it => {
    set table(
      stroke: (x, y) => (
        left: stroke-width + stroke-color,
        right: stroke-width + stroke-color,
        top: if y == 0 { stroke-width + stroke-color } else { none },
        bottom: stroke-width + stroke-color,
      ),
    )

    // Style the table
    show table.cell.where(y: 0): set text(weight: "bold")
    show table.cell.where(y: 0): set align(align-header)
    show table.cell.where(y: 0): set fill(header-fill)

    it
  }
}

// ============================================
// MAIN TEMPLATE FUNCTION
// ============================================

/// Main academic paper template
///
/// Parameters:
///   - title: Paper title (required)
///   - author: Author name (required)
///   - student-id: Student ID (optional, for academic submissions)
///   - course: Course code and name (optional)
///   - date: Document date (default: today)
///   - header-text: Custom header text (default: uses course or title)
///   - abstract: Paper abstract (optional)
///
///   Font options:
///   - font: Body text font (default: "Liberation Sans")
///   - font-size: Body text size (default: 12pt)
///   - heading-font: Heading font (default: same as body font)
///
///   Spacing options:
///   - line-spacing: Line spacing within paragraphs in em (default: 0.75em)
///   - first-line-indent: Paragraph first line indent (default: 0in, no indent)
///   - paragraph-spacing: Space between paragraphs (default: 2em)
///
///   Page options:
///   - paper: Paper size (default: "us-letter")
///   - margin: Page margins (default: 1in all sides)
///   - page-numbering: Numbering format (default: "1")
///
///   Document structure:
///   - show-title-page: Include title page (default: true)
///   - show-toc: Include table of contents (default: true)
///   - toc-title: Table of contents title (default: "Table of Contents")
///   - toc-depth: TOC depth (default: 3 levels)
///   - toc-indent: TOC indent style (default: auto)
///
///   Heading options:
///   - heading-numbering: Heading numbering format (default: none)
///   - heading-numbering-levels: Number of levels to number (default: 5)
///
///   Text options:
///   - justify: Justify body text (default: true)
///   - hyphenate: Enable hyphenation (default: true)
///
///   Bibliography options:
///   - bibliography-file: Path to bibliography file (optional)
///   - bibliography-style: Citation style (default: "apa")
///   - bibliography-title: Bibliography section title (default: "References")
///
///   Glossary options (requires glossarium package):
///   - glossary-file: Path to glossary/acronyms file (optional)
///   - glossary-title: Glossary section title (default: "Glossary")
///
///   Body: Document content
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

  // ============================================
  // DOCUMENT METADATA
  // ============================================

  set document(
    title: title,
    author: author,
    date: date,
  )

  // ============================================
  // PAGE SETUP
  // ============================================

  set page(
    paper: paper,
    margin: margin,
    numbering: page-numbering,
    header: if header-text != none {
      align(right)[_#header-text _]
    },
  )

  // ============================================
  // TEXT AND PARAGRAPH FORMATTING
  // ============================================

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

  // ============================================
  // LIST FORMATTING
  // ============================================

  // Set list indentation with increasing indent per level
  set list(indent: 1em, body-indent: 0.5em)
  set enum(indent: 1em, body-indent: 0.5em)

  // ============================================
  // HEADING FORMATTING
  // ============================================

  set heading(numbering: heading-numbering)

  show heading: it => {
    set text(font: heading-font)
    it
    v(0.5em, weak: true)
  }

  // Level 1 headings (chapters/main sections)
  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    set text(size: 16pt, weight: "bold")
    it
    v(1em)
  }

  // Level 2 headings
  show heading.where(level: 2): it => {
    set text(size: 14pt, weight: "bold")
    it
    v(0.75em)
  }

  // Level 3 headings
  show heading.where(level: 3): it => {
    set text(size: 12pt, weight: "bold")
    it
    v(0.5em)
  }

  // Level 4+ headings
  show heading.where(level: 4): it => {
    set text(size: 12pt, weight: "bold", style: "italic")
    it
  }

  // ============================================
  // BIBLIOGRAPHY SETUP (if provided)
  // ============================================

  if bibliography-file != none {
    // Set bibliography style
    set bibliography(title: bibliography-title, style: bibliography-style)
  }

  // ============================================
  // TITLE PAGE
  // ============================================

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

  // ============================================
  // ABSTRACT (if provided)
  // ============================================

  if abstract != none {
    align(center)[
      #text(size: 14pt, weight: "bold")[Abstract]
    ]

    v(1em)

    set par(first-line-indent: 0in)
    abstract

    pagebreak()
  }

  // ============================================
  // TABLE OF CONTENTS
  // ============================================

  if show-toc {
    outline(
      title: toc-title,
      depth: toc-depth,
      indent: toc-indent,
    )

    pagebreak()
  }

  // ============================================
  // MAIN CONTENT
  // ============================================

  body

  // ============================================
  // BIBLIOGRAPHY (if file provided)
  // ============================================

  if bibliography-file != none {
    pagebreak()

    // Reset paragraph formatting for bibliography (APA style)
    set par(
      hanging-indent: 0.5in,
      first-line-indent: 0in,
    )

    bibliography(bibliography-file)
  }

  // ============================================
  // GLOSSARY FORMATTING
  // ============================================

  // Note: Glossary display handled in main document using print-glossary()
  // This section provides styling rules for glossary content

}

// ============================================
// UTILITY FUNCTIONS
// ============================================

/// Create a properly formatted figure with caption
///
/// Parameters:
///   - content: Figure content (image, table, etc.)
///   - caption: Figure caption
///   - label: Optional label for cross-referencing
///   - placement: Figure placement (default: auto)
#let academic-figure(content, caption, label: none, placement: auto) = {
  figure(
    content,
    caption: caption,
    placement: placement,
  )
  if label != none {
    label
  }
}

/// Create a block quote with proper formatting
///
/// Parameters:
///   - content: Quote content
///   - attribution: Quote attribution (optional)
#let block-quote(content, attribution: none) = {
  set par(first-line-indent: 0in)
  block(
    inset: (left: 0.5in, right: 0.5in),
    [
      #content

      #if attribution != none [
        #align(right)[
          #text(style: "italic")[— #attribution]
        ]
      ]
    ]
  )
}

/// Create a highlighted note/info box
///
/// Parameters:
///   - content: Box content
///   - title: Box title (optional)
///   - fill-color: Background color (default: light gray)
///   - radius: Corner radius (default: 4pt)
#let info-box(content, title: none, fill-color: rgb("#f0f0f0"), radius: 4pt) = {
  block(
    fill: fill-color,
    inset: 10pt,
    radius: radius,
  )[
    #if title != none [
      #text(weight: "bold")[#title]
      #v(0.5em)
    ]
    #content
  ]
}

/// Format glossary/acronyms as a professional table
///
/// Parameters:
///   - entries: Array of glossary entries (from glossarium format)
///   - show-group: Show the group column (default: false)
///   - show-plural: Show the plural column (default: false)
///
/// Usage:
///   #glossary-table(acronyms)
#let glossary-table(entries, show-group: false, show-plural: false) = {
  // Build column specification based on what to show
  let cols = if show-group and show-plural {
    (auto, 1fr, 2fr, auto, auto)
  } else if show-group {
    (auto, 1fr, 2fr, auto)
  } else if show-plural {
    (auto, 1fr, 2fr, auto)
  } else {
    (auto, 1fr, 2fr)
  }

  // Build header row
  let header = if show-group and show-plural {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Plural*], [*Group*])
  } else if show-group {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Group*])
  } else if show-plural {
    ([*Abbreviation*], [*Long Form*], [*Description*], [*Plural*])
  } else {
    ([*Abbreviation*], [*Long Form*], [*Description*])
  }

  // Create table
  table(
    columns: cols,
    stroke: 0.5pt,
    fill: (x, y) => if y == 0 { rgb("#e8e8e8") },
    align: (x, y) => if y == 0 { center } else { left },
    inset: 8pt,

    // Header row
    ..header,

    // Data rows
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
