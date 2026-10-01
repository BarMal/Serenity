package com.serenity.command

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.BufferId

enum FileIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  case OpenFile
  case OpenRecentFile(path: Path)
  case OpenFileSearch
  case CloseAll
  case CloseOthers
  case CloseCurrentFile
  case NewFile
  case SetBufferLanguage(language: Option[LanguageId])
  // #1623: the ways out of a file changed on disk under unsaved edits -- see ExternalChangeCommands.
  case ReloadFromDisk(bufferId: BufferId)
  case OverwriteOnDisk(bufferId: BufferId)
  case SaveWithoutFormatting(bufferId: BufferId)
