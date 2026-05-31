package com.serenity.lsp.config

import io.circe.Json

case class LspServerConfig(
  languageId:            LanguageId,
  binary:                LspServerBinary,
  defaultArgs:           List[String]  = Nil,
  initializationOptions: Json          = Json.Null
)
