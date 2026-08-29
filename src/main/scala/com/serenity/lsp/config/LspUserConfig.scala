package com.serenity.lsp.config

import pureconfig.ConfigReader

final case class LspServerOverride(
    command: Option[String],
    args: Option[List[String]],
    enabled: Option[Boolean] = None
) derives ConfigReader

final case class LspUserConfig(
    servers: Option[Map[String, LspServerOverride]]
) derives ConfigReader

object LspUserConfig:
  val empty: LspUserConfig = LspUserConfig(servers = None)
