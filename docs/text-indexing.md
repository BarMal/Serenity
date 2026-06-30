# Text Indexing Semantics

## Decision

Serenity stores buffer offsets and cursor columns as UTF-16 code unit indexes.

The rope, Java `String`, Swing/AWT text measurement, persistence, search ranges, and current
line/column conversion APIs all use the same indexing unit. Keeping the internal contract UTF-16
avoids lossy translation at storage and rendering boundaries.

User-facing text editing must not expose raw code-unit movement when that would split a visible
character. Cursor-left, cursor-right, forward delete, and backward delete therefore route through
`TextEditing` grapheme-boundary helpers before converting back to internal UTF-16 offsets. Selection
replacement and deletion also expand non-empty ranges outward to grapheme boundaries, so an invalid
or externally restored selection cannot replace only part of a visible character.

## Consequences

- Internal offsets may advance by more than one when a grapheme spans multiple UTF-16 code units.
- Regression tests should assert both the visible edit result and the UTF-16 cursor column.
- APIs accepting offsets or columns should document whether they expect internal UTF-16 indexes or
  user-facing grapheme steps.
- Empty insertions still occur at the supplied UTF-16 cursor position; callers that create cursors
  from mouse, persisted, or external positions should snap them to grapheme boundaries before editing.
