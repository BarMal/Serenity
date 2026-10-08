# Exporting a manuscript

Serenity can turn the document you are editing, or a whole book, into a Word manuscript (DOCX) or an e-book (EPUB).
Export from the command palette:

- **Export Manuscript...** asks which format.
- **Export Manuscript as DOCX...** and **Export Manuscript as EPUB...** go straight to a format.

Unsaved edits are included. The suggested file name is the document's name plus `-manuscript` (`novel.md` becomes
`novel-manuscript.docx`), so an export never offers to overwrite the draft it came from.

## One document or a book

With no `manuscript.conf`, the book is the current document. To export several files as one book, put a
`manuscript.conf` in the same folder as the document you are editing. It lists the files in order:

```
title = "The Long Night"
author = "Jane Q. Writer"
sources = [
  { path = "01-arrival.md" },
  { path = "02-storm.md" },
  { path = "notes.md", exclude = true }
]
```

`exclude = true` keeps a file listed but out of the export. Every key is optional. The others are:

| Key | Meaning |
| --- | --- |
| `short-title` | Running-header keyword. Defaults to the title in capitals. |
| `surname` | When the last word of the author's name is not the surname. |
| `byline`, `contact` | Byline, and contact lines for the title page. |
| `format` | `modern` or `classic`. |
| `paper` | `letter` or `a4`. |
| `chapter-heading`, `part-heading` | Heading templates, such as `"Chapter <$n>\n<$t>"`. |
| `part-level`, `chapter-level` | Which heading levels start a part and a chapter. |
| `scene-break-patterns` | Lines that count as a scene break, such as `["#", "***"]`. |
| `transforms`, `replacements` | Text changes applied on export, such as `smart-punctuation`, and find-and-replace pairs. |
| `title-page`, `dedication`, `end-marker` | Front and back matter. |
| `word-count` | `novel`, `short-fiction` or `exact`. |
| `language`, `identifier` | EPUB language tag and permanent book id. |

If the sources or settings cannot be compiled into a manuscript, the export reports the reason and writes no file.
