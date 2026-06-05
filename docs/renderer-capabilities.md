# Renderer Capabilities

Generated on 2026-06-05 against the current branch state.

This document describes what Serenity's renderer can do today, where the sharp edges are, and what those limits mean for Markdown and prose features.

## Core Model

- The editor renderer draws editor panes, pinned panels, floating overlays, gutters, line numbers, cursors, and selections from a single immutable `AppState` snapshot.
- Each editor pane points at one buffer. The renderer computes one `TextLayoutSnapshot` per visible buffer pane so text, selections, and cursors use the same geometry.
- Buffer content is rendered as visual lines. A visual line may be a wrapped slice of one logical buffer line.
- Rendering is cell-oriented for stable monospaced text and pixel-oriented for proportional text, ligature-enabled fonts, or fonts whose measured advances drift from cell metrics.

## Text Geometry

- `TextLayoutSnapshot` owns visual-line wrapping, caret stops, measured widths, line height, and ascent for a visible buffer slice.
- A snapshot has one line height and one ascent. Mixed per-line metrics are not supported today.
- Proportional and ligature-aware text can use measured caret stops, so cursor placement and selection painting can follow the same geometry as the drawn glyphs.
- Horizontal viewport calculations for measured text are based on caret positions rather than simple character cells.

## Fonts

- Buffers choose either the code font or the text font.
- Plain-text and Markdown buffers use the text-font path.
- Code-oriented language buffers use the code-font path.
- UI surfaces use the UI font and UI metrics separately from editor buffer content.
- The renderer can switch fonts per pane/buffer, but not within one line or block in a way that changes layout metrics.

## Styling

- `TextStyle` supports bold, italic, and underline.
- Syntax and Markdown highlighting are represented as styled text segments.
- Java2D rendering applies bold, italic, and underline through derived `Font` attributes.
- Styling can vary within a line, but line height and ascent do not vary with that styling.

## Markdown

- Markdown is currently an editor-first presentation mode.
- Existing Markdown styling recognizes headings, unordered and ordered list markers, blockquotes, inline code, and links.
- Markdown block-lens rendering keeps the active Markdown block as raw source while surrounding Markdown lines use the existing Markdown presentation styling.
- The current block detector recognizes paragraphs, list regions with indented continuations, blockquote regions, fenced code blocks, tables, and blank lines.
- The lens is same-metric: it changes styling decisions, not text layout, line height, heading size, or block spacing.

## Selections And Cursors

- Cursors are rendered from the same snapshot used to draw text.
- Selections over measured text are drawn with pixel-accurate ranges from caret stops.
- Selections over fixed-cell text are drawn in cell coordinates.
- The renderer currently assumes selected text has the same character positions as the buffer text. Presentation modes that hide or reflow source markers need a deeper selection/cursor model before they are safe.

## Overlays And Panels

- Floating overlays and pinned panels render as row-based text surfaces.
- Overlay rows support plain, split, and distributed layouts.
- Long plain overlay rows can scroll horizontally to keep their cursor visible.
- Overlay and panel rendering is useful for menus, workflows, previews, diagnostics, outlines, and simple text surfaces.
- Overlay rows are not a rich document renderer and do not support full Markdown document layout.

## Unsupported Today

- Per-heading font sizes inside the editor pane.
- Mixed line heights or block spacing inside one buffer snapshot.
- Inline rendered tables, images, or rich document objects.
- A full Markdown preview surface.
- Hidden-source marker rendering that preserves selection and cursor semantics.
- A general embedded multi-line editor or sub-viewport inside an editor pane.
- Browser-grade HTML/CSS rendering inside editor content.

## Likely Future Directions

- Use block-aware Markdown parsing to improve the current lens while keeping raw-source editing predictable.
- Add a separate preview surface if Serenity needs true rendered Markdown documents.
- Consider a browser-backed renderer, pure-Java HTML renderer, or source-aware Markdown parser only after the desired preview model is clear.
- Introduce per-block or mixed-metric layout only after the snapshot and selection models can represent it directly.
