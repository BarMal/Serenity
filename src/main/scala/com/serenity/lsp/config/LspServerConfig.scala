package com.serenity.lsp.config

import io.circe.Json

final case class LspServerConfig(
    languageId: LanguageId,
    binary: LspServerBinary,
    defaultArgs: List[String] = Nil,
    initializationOptions: Json = Json.Null,
    commandOverride: Option[String] = None
):
  def command: String =
    commandOverride.getOrElse(binary.command)
