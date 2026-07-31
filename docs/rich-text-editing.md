# Rich Text Editing Direction

## Goal

Serenity should treat rich text as a first-class document model rather than as Markdown with extra syntax. Markdown remains useful for preview and interchange, but it cannot preserve enough information for `.doc`, `.odt`, or future rich editing features without losing round-trip fidelity.

## Preferred Architecture

Use a native rich text model as the canonical representation:

- `RichTextDocument` owns paragraphs.
- `RichTextParagraph` owns paragraph-level formatting such as alignment.
- `RichTextRun` owns inline style such as bold, italic, underline, font family, size, and colour.
- File-format adapters read/write external formats into this model.
- Rendering and editing code operate on the model directly.

## Markdown Reuse Trade-Off

Reusing the Markdown renderer is cheaper for read-only previews and simple inline styling, but it is a poor canonical model for rich editing.

Pros:

- Lower initial rendering cost for bold, italic, headings, lists, and tables.
- Existing Markdown preview work can help with lightweight import previews.
- Useful fallback for formats that can tolerate lossy conversion.

Cons:

- Markdown cannot reliably represent document-format details such as underline, font family, font size, exact paragraph alignment, colour, spacing, page-level metadata, or unknown vendor extensions.
- Cursor placement and selection become lossy when one source character sequence expands into rendered formatting.
- Round-tripping `.doc` or `.odt` through Markdown risks silently deleting formatting the user expected to preserve.
- Editing styled runs needs model-level operations; syntax-level Markdown transforms are not enough.

## Decision

Build rich text support around the native model and keep Markdown reuse as an adapter or preview fallback only. That gives Serenity a stable target for `.doc`, `.odt`, clipboard formats, and future native rendering while still allowing cheap preview paths where fidelity is explicitly not required.

## DOCX and ODT Fidelity Contract

The native DOCX and ODT adapters losslessly represent paragraphs, headings, alignment, inline text marks, font metadata, tabs, line breaks, and the archive entries used by Serenity's writers. Tables, lists, images, links, headers, footnotes, metadata, comments, tracked changes, and other package extensions are outside that model. The adapters expose this boundary as `RichTextFidelity`; DOCX and ODT are therefore advertised as editable but not fully rich-format preserving.

The current codec slice detects unsupported imported structures before a caller saves. Buffer/session retention and an explicit lossy-save or Save As decision remain application-layer work. No Apache POI or ODF Toolkit dependency is required for this contract because detection operates on the existing XML/package reader.
