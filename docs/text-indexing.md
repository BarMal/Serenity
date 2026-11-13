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
Authored document comments normalize their creation ranges through the same boundaries, so comment
spans cannot split a visible character.

## Columns shown to the user

The stored column is never shown as it is. The status line's `Col` is the **visual column** (#1967): the cells before
the cursor on its line, plus one. A tab takes the cells up to the next tab stop (`TabStops.cellsAt`, with the buffer's
tab size), a wide character or an emoji cluster takes the cells it occupies (`CharWidth.ofCluster`), and a base with its
combining marks takes one cell. `DisplayColumn.of` (`ui/layout/DisplayColumn.scala`) computes it from the line's text
and the stored column. A column inside a cluster counts only the clusters before it, since the caret cannot stand there.
So `a` followed by a flag (four UTF-16 units) puts the cursor at stored column 5 and visual `Col 4`.

The symbol-definition peek (`SurfaceContentResolver`) prints the server's own position plus one and is not a visual
column.

## Other units

- **File bytes.** `TextEncoding` records how a file was encoded on disk (UTF-8, UTF-16, Windows-1252, ISO-8859-1) so
  saving writes the same bytes. The rope holds decoded text, so every column and offset is UTF-16 whatever the file was.
- **Language servers.** An LSP `Position.character` is counted in the encoding the server and client agreed on
  (`PositionEncoding`). Serenity offers only UTF-16, the same unit as the stored column, so positions are used as they
  arrive; positions in another encoding cannot be used as they arrive (`lsp/model/PositionEncoding.scala`).
- **Graphemes.** `TextEditing` grapheme-boundary helpers convert between a user-facing step and the UTF-16 offset.

## Consequences

- Internal offsets may advance by more than one when a grapheme spans multiple UTF-16 code units.
- Regression tests should assert both the visible edit result and the UTF-16 cursor column.
- APIs accepting offsets or columns should document whether they expect internal UTF-16 indexes or
  user-facing grapheme steps.
