package com.serenity.command

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.BufferId
import com.serenity.text.{LineEnding, TextEncoding}

enum FileIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  // #1206: compile the focused document, or the book its manuscript.conf lists, to a manuscript file.
  case ExportManuscript(request: ManuscriptExportRequest)
  case OpenFile
  case OpenFolder
  case OpenFileOrFolder
  case OpenRecentFile(path: Path)
  case OpenRecentFolder(path: Path)
  case ClearRecentFiles
  case OpenFileSearch
  case GoToFile
  case CloseAll
  case CloseOthers
  case CloseCurrentFile
  case NewFile
  // #2019: the GPL-3.0-or-later licence and third-party notices bundled in the jar, opened read-only.
  case ShowLicenceAndNotices
  // The About Serenity document; show-licence-and-notices opens the same document.
  case ShowAbout
  // Option B of the update notice: the browser makes the request, Serenity opens no socket.
  case OpenReleasesPage
  // The privacy statement (docs/PRIVACY.md) bundled in the jar, opened read-only.
  case ShowPrivacyStatement
  case SetBufferLanguage(language: Option[LanguageId])
  // #1623: the ways out of a file changed on disk under unsaved edits -- see ExternalChangeCommands.
  case ReloadFromDisk(bufferId: BufferId)
  case OverwriteOnDisk(bufferId: BufferId)
  case SaveWithoutFormatting(bufferId: BufferId)
  // #1627: read the file again in an encoding the user picks -- see ReopenWithEncodingCommands.
  case ChooseReopenEncoding
  case ReopenWithEncoding(bufferId: BufferId, encoding: TextEncoding, discardEdits: Boolean)
  // #1964: change the line ending a buffer saves with -- see LineEndingCommands.
  case ChooseLineEnding
  case SetLineEnding(bufferId: BufferId, ending: LineEnding)
