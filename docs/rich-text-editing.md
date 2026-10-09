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

## DOCX, ODT and RTF Fidelity Contract

The native DOCX and ODT adapters losslessly represent paragraphs, headings, alignment, inline text marks, font metadata, tabs, line breaks, and the archive entries used by Serenity's writers. Tables, lists, images, links, headers, footnotes, metadata, comments, tracked changes, and other package extensions are outside that model. The adapters expose that boundary as a per-feature `FidelityReport` (read-only, preserved, converted, dropped and removed counts for each save target), checked on import (`LossyRichTextOverwriteException`, issue #856).

That import-time boundary is a separate axis from `DocumentFormat.capabilities(_).preservesRichFormatting`, which asks whether saving a buffer's *currently authored* formatting -- marks, alignment, headings -- survives at a given target format. RTF, ODT, and DOCX all report `true` there: every one of Serenity's own codecs round-trips marks, alignment, and headings without loss (issues #1291, #1887).

RTF is read and written by Serenity's own tokenizer, parser and writer (`RtfTokenizer`, `RtfParser`, `RtfReader`, `RtfWriter`), not by Swing's `RTFEditorKit`. It decodes `\uN` with `\ucN` skip counts and `\'hh` in the declared code page (or the current font's charset), `\line`, `\tab`, paragraph and character styles from `\stylesheet`, headings from `\outlinelevel` or `heading N` style names, drop caps from `\dropcapliN`, the font and colour tables, and character formatting (bold, italic, underline, font, size, colour). It writes 7-bit ASCII RTF with headings as real `heading N` styles. Malformed or truncated input is a `Left`, never an exception. Reported through `FidelityReport` as dropped (so an in-place save is refused): tables, pictures, embedded objects, shapes, footnotes, headers and footers, lists, fields (their text is kept, the instruction is not), comments, strikethrough, super/subscript, highlight, and any unknown `\*` destination (`destination:name`). Soft line breaks remain `\n` inside a run, as in DOCX and ODT.

The current codec slice detects unsupported imported structures before a caller saves. Buffer/session retention and an explicit lossy-save or Save As decision remain application-layer work. No Apache POI or ODF Toolkit dependency is required for this contract because detection operates on the existing XML/package reader.

### Package passthrough (issue #1896)

A document read from a DOCX or ODT package keeps that package (`RichTextDocument.source`). Saving it in the same format writes the package back rather than a new one: every part the model does not own (styles, settings, theme, comments, media, relationships, metadata) is copied with the same bytes, order, compression method and timestamp, and the main part is rebuilt from the original text. A body paragraph whose model value still equals what was imported is copied from its source slice byte for byte; only edited or new paragraphs are written again. Body children that are not paragraphs (tables, section properties, sequence declarations) stay where they were between the paragraphs that surround them.

A rewritten paragraph keeps what the model cannot hold. Paragraph properties and the `w:p` attributes travel on the paragraph (`ParagraphSource`), run properties on the run's style (`RichTextStyle.extras`), and inline content with no model (images, bookmarks, comment anchors, fields, footnote references, ODT annotations and frames) is an opaque atom (`InlineAtom.Opaque`) that takes one rope character, so line `i` is still paragraph `i`. Provenance is not content: paragraph equality ignores it, and it is not written to session files. Raw XML is only ever written back into the format it came from; saving to another format drops opaque atoms.

A body child the model does not hold (a table, content control, equation, index, tracked change, ODT list or section) is one read-only line: a paragraph holding a single `InlineAtom.Block`, whose rope character is U+2064 (not U+FFFC, which a plain text export turns into a newline). It is written back byte for byte. An edit that would put content beside it is refused; deleting the whole line removes the block, and the report says "removed". Plain text exports (copy, counts, accessibility) leave the placeholder out through `InlineAtom.asPlainText`.

In-place save is gated on `FidelityReport.wouldDrop` for the target format, so a table kept by passthrough no longer blocks it, while Save As shows the summary ("1 table preserved read-only"). A session stores no package bytes: on restore the file is read again and re-linked only if it is still at the recorded revision, otherwise the document is detached and the warning lists what would be lost.

The golden harness (`src/test/resources/richtext/golden/<fixture>/`, `RichTextGoldenSpec`) holds a source package, scripted edits and the expected main part. It checks that an unedited document round-trips with every entry, its order and its bytes, and that an edit changes only the touched paragraph's slice. The fixtures are authored for this project and rebuilt by `GoldenFixtures` (`sbt "Test / runMain com.serenity.richtext.GoldenFixtures"`). Apache POI (Apache-2.0) is a test-scope dependency only: the specs open every written DOCX in XWPF as an independent check.
