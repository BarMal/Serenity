# Text Indexing Semantics

## Decision

Serenity stores buffer offsets and cursor columns as UTF-16 code unit indexes.

The rope, Java `String`, Swing/AWT text measurement, persistence, search ranges, and current
line/column conversion APIs all use the same indexing unit. Keeping the internal contract UTF-16
avoids lossy translation at storage and rendering boundaries.

User-facing text editing must not expose raw code-unit movement when that would split a visible
character. Cursor-left, cursor-right, forward delete, and backward delete therefore route through
`TextEditing` grapheme-boundary helpers before converting back to internal UTF-16 offsets. Empty
insertions snap to the grapheme boundary after the supplied cursor position, and selection replacement
or deletion expands non-empty ranges outward to grapheme boundaries. Invalid or externally restored
cursor and selection positions therefore cannot edit only part of a visible character.
Measured text layout exposes caret stops only at grapheme boundaries, so rendering and pixel hit
testing also avoid placing cursors inside a visible character.

## Consequences

- Internal offsets may advance by more than one when a grapheme spans multiple UTF-16 code units.
- Regression tests should assert both the visible edit result and the UTF-16 cursor column.
- APIs accepting offsets or columns should document whether they expect internal UTF-16 indexes or
  user-facing grapheme steps.
