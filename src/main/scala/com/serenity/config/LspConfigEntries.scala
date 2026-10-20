package com.serenity.config

import java.util.Locale

import scala.jdk.CollectionConverters.*

import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.typesafe.config.{Config, ConfigValueType}

/** The `lsp.<language>.<field>` keys of the config file, which are a family rather than one registered field each. */
private[config] object LspConfigEntries:

  def applyLists(config: AppConfig, source: Config): AppConfig =
    source.entrySet().asScala.foldLeft(config) { (current, entry) =>
      val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
      if key.startsWith("lsp.") && key.endsWith(".args") && entry.getValue.valueType == ConfigValueType.LIST then
        key.split("\\.", 3).toList match
          case "lsp" :: languageKey :: "args" :: Nil =>
            LanguageId.fromString(languageKey).fold(current) { languageId =>
              val args = source.getList(entry.getKey).asScala.map(_.unwrapped().toString).toList
              updateLspOverride(current, languageId)(_.copy(args = Some(args)))
            }
          case _ => current
      else current
    }

  def parse(config: AppConfig, key: String, value: String): AppConfig =
    key.split("\\.", 3).toList match
      case "lsp" :: languageKey :: field :: Nil =>
        LanguageId.fromString(languageKey).fold(config) { languageId =>
          field match
            case "enabled" =>
              parseBoolean(value)
                .map(enabled => updateLspOverride(config, languageId)(_.copy(enabled = Some(enabled))))
                .getOrElse(config)
            case "command" =>
              updateLspOverride(config, languageId)(_.copy(command = Option(value).filter(_.nonEmpty)))
            case "args" =>
              val args = value
                .split(",")
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
              updateLspOverride(config, languageId)(_.copy(args = Some(args)))
            case _ =>
              config
        }
      case _ =>
        config

  private def updateLspOverride(
    config: AppConfig,
    languageId: LanguageId
  )(update: LspServerOverride => LspServerOverride): AppConfig =
    val servers  = config.languageToolsConfig.lspUserConfig.servers.getOrElse(Map.empty)
    val existing = servers.getOrElse(languageId.id, LspServerOverride(command = None, args = None))
    config.withLspUserConfig(
      LspUserConfig(
        servers = Some(servers + (languageId.id -> update(existing)))
      )
    )

  def parseBoolean(value: String): Option[Boolean] =
    value.toLowerCase match
      case "true" | "on" | "enabled"    => Some(true)
      case "false" | "off" | "disabled" => Some(false)
      case _                            => None
