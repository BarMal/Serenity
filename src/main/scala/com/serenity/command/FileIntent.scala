package com.serenity.command

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.BufferId
import com.serenity.text.TextEncoding

enum FileIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  // #1206: compile the focused document, or the book its manuscript.conf lists, to a manuscript file.
  case ExportManuscript(request: ManuscriptExportRequest)
  case OpenFile
  case OpenRecentFile(path: Path)
  case OpenFileSearch
  case GoToFile
  case CloseAll
  case CloseOthers
  case CloseCurrentFile
  case NewFile
  case SetBufferLanguage(language: Option[LanguageId])
  // #1623: the ways out of a file changed on disk under unsaved edits -- see ExternalChangeCommands.
  case ReloadFromDisk(bufferId: BufferId)
  case OverwriteOnDisk(bufferId: BufferId)
  case SaveWithoutFormatting(bufferId: BufferId)
  // #1627: read the file again in an encoding the user picks -- see ReopenWithEncodingCommands.
  case ChooseReopenEncoding
  case ReopenWithEncoding(bufferId: BufferId, encoding: TextEncoding, discardEdits: Boolean)
