package com.serenity.command

import com.serenity.manuscript.ManuscriptFileFormat

/** Exporting the focused document as a manuscript (#1206): one palette entry that asks for the format, and one direct
  * command per format for key bindings and scripts.
  */
/** What an "Export Manuscript" command asks for: the format question, or a format already chosen. */
enum ManuscriptExportRequest:
  case ChooseFormat
  case As(format: ManuscriptFileFormat)

object ManuscriptExportCommands:

  val choose: Command =
    Command.typed(
      "export-manuscript",
      "Export the current document, or the book its manuscript.conf lists, as a DOCX manuscript or an EPUB e-book.",
      CommandIntent.File(FileIntent.ExportManuscript(ManuscriptExportRequest.ChooseFormat)),
      CommandCategory.File,
      label = "Export Manuscript..."
    )

  def exportAs(format: ManuscriptFileFormat): Command =
    Command.typed(
      s"export-manuscript-${format.key}",
      s"Export the current document, or the book its manuscript.conf lists, as ${format.label}.",
      CommandIntent.File(FileIntent.ExportManuscript(ManuscriptExportRequest.As(format))),
      CommandCategory.File,
      label = s"Export Manuscript as ${format.key.toUpperCase}..."
    )

  val all: List[Command] = choose :: ManuscriptFileFormat.values.toList.map(exportAs)
