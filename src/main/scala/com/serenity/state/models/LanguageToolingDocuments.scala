package com.serenity.state.models

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope

/** The open documents a language server is told about: those with a language and a file the large-file policy leaves
  * language tooling for, one per file, in buffer order.
  */
object LanguageToolingDocuments:

  final case class Announced(path: Path, language: LanguageId, text: Rope):
    def uri: String = path.toUri.toString

  def of(state: AppState): List[Announced] =
    state.persisted.buffers.values.toList
      .sortBy(_.id.value)
      .flatMap { buffer =>
        for
          path     <- buffer.document.filePath
          language <- buffer.document.language
          if state.persisted.config.largeFiles.allowsLanguageTooling(buffer.document)
        yield Announced(path, language, buffer.document.content)
      }
      .distinctBy(_.path)
