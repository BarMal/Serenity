package com.serenity.lsp.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

case class LspServerOverride(
    command: Option[String],
    args: Option[List[String]]
) derives ConfigReader

case class LspUserConfig(
    servers: Option[Map[String, LspServerOverride]]
) derives ConfigReader

object LspUserConfig:
  val empty: LspUserConfig = LspUserConfig(servers = None)
