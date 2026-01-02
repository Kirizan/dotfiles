// Example: Converting your current D778 paper to use the template
//
// This example shows how to convert from inline formatting to the
// academic-paper template for cleaner, more maintainable code.
//
// Copy this structure and adapt it for your papers.

// ============================================
// IMPORTS
// ============================================

// Import the template (adjust path as needed)
#import "academic-paper.typ": academic-paper, info-box, block-quote, table-style

// Import glossarium for acronyms
#import "@preview/glossarium:0.5.9": make-glossary, print-glossary, register-glossary
#import "acronyms.typ": acronyms

// ============================================
// GLOSSARIUM SETUP (must be before template)
// ============================================

#show: make-glossary
#register-glossary(acronyms)

// ============================================
// APPLY TEMPLATE
// ============================================

#show: academic-paper.with(
  // Required fields
  title: "D778 - Advanced Software Engineering",
  author: "Nicholas Kirby",

  // Academic metadata
  student-id: "000690574",
  course: "D778 Advanced Software Engineering",
  date: datetime.today(),

  // Font configuration
  font: "Liberation Sans",
  font-size: 12pt,

  // Spacing (double-spaced, 0.5in indent)
  line-spacing: 2em,
  first-line-indent: 0.5in,

  // Page setup
  paper: "us-letter",
  margin: 1in,
  page-numbering: "1",

  // Document structure
  show-title-page: true,
  show-toc: true,
  toc-title: "Table of Contents",
  toc-depth: 3,

  // Heading configuration
  heading-numbering: none,

  // Text formatting
  justify: true,
  hyphenate: true,

  // Bibliography
  bibliography-file: "bibliography/bibliography.yml",
  bibliography-style: "apa",
  bibliography-title: "G. References",
)

// ============================================
// DOCUMENT CONTENT
// ============================================

= Part I: Software Development Methodology

== A. Methodology Comparison

Compare the agile and waterfall methodologies regarding the proposed development of the e-commerce web application.

=== A.1. Phases, Activities, and Deliverables

For each methodology, describe the typical phases, activities, and deliverables of that methodology.

==== A.1.a. Waterfall Methodology

Describe the typical phases of Waterfall.

===== _Requirements gathering_

According to Tutorials Point, the requirements gathering phase is: "All possible requirements of the system to be developed are captured in this phase and documented in a requirement specification document." @tutorialspoint_0000_01

The requirements gathering phase consist of the following activities:
- Identifying Stakeholders and ensuring a project @raci exists.
- Gather all possible requirements for stakeholders.
- Analyze requirements to ensure they are both complete and consistent.
- Document the requirements in the @srs
- Define project scope, stakeholders, roadmap, team, and deadlines.

The following artifacts are output of the Requirements Gathering phase:
- @srs:long document
- Functional Specification document
- Non-functional requirement document
- @raci:long

// ... rest of your content ...

== B. Methodology Selection Justification

// Use the info-box utility for highlighting important sections
#info-box(
  title: "Relevant References for this section:",
)[
  - PMI Agile Practice Guide (comprehensive methodology guidance)
  - Digital.ai State of Agile Report (industry adoption data)
  - Standish Group CHAOS Report (project success factors)
  - Shopify/BigCommerce Development Guides (e-commerce context)
  - Scaled Agile Framework (enterprise considerations)
]

// ... more content ...

// ============================================
// GLOSSARY (after bibliography, which is auto-added)
// ============================================

#pagebreak()

= Glossary

#print-glossary(acronyms)

// ============================================
// NOTES ON MIGRATION
// ============================================

/*
  What changed from your original main.typ:

  REMOVED (now handled by template):
  ✓ #set document(...)
  ✓ #set page(...)
  ✓ #set text(...)
  ✓ #set par(...)
  ✓ #set heading(...)
  ✓ Title page formatting
  ✓ Table of contents
  ✓ Manual bibliography set rules

  KEPT (your content):
  ✓ All headings and content
  ✓ Acronym references (@cpu, @srs, etc.)
  ✓ Citations (@tutorialspoint_0000_01, etc.)
  ✓ Custom info boxes and formatting

  BENEFITS:
  ✓ Cleaner, more readable main.typ
  ✓ Reusable across all future papers
  ✓ Centralized formatting updates
  ✓ Easier to experiment with different styles
  ✓ Version controlled in dotfiles
  ✓ Synchronized across machines via chezmoi
*/
