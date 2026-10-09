package com.serenity.config

import java.util.Locale

import scala.jdk.CollectionConverters.*

import com.typesafe.config.{Config, ConfigValueType}

/** Version 1 files wrote every `hotkey.<action>` with the full list the writing build held, so a list in one is
  * evidence of a build's defaults rather than of a choice. This drops the ones that are no more than the current
  * defaults, leaving the rest as the user's own.
  *
  * An action list counts as default when every trigger in it is one the current default list holds. That covers the
  * list as it was before a trigger was added (redo before Ctrl+Shift+Z) and an empty list, which version 1 ignored. A
  * trigger the user removed from a default list on purpose cannot be told apart from one that was never there yet, so
  * it comes back once; removing it again writes an override that survives. Command bindings are left alone: an empty
  * one has always meant "unbound", and a shipped command default has only ever held one trigger.
  */
private[config] object HotkeyMigration:

  def withoutDefaultBindings(osName: String)(config: Config): Config =
    val defaults = HotkeyConfig.platformDefaults(osName)
    config.entrySet().asScala.toList.map(_.getKey).foldLeft(config) { (kept, path) =>
      if isDefault(config, path, defaults) then kept.withoutPath(path) else kept
    }

  private def isDefault(config: Config, path: String, defaults: Map[HotkeyAction, List[HotkeyTrigger]]): Boolean =
    val key = path.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
    HotkeyAction.values.find(action => key == s"hotkey.${action.configKey}").exists { action =>
      triggersAt(config, path).exists(_.toSet.subsetOf(defaults.getOrElse(action, Nil).toSet))
    }

  private def triggersAt(config: Config, path: String): Option[List[HotkeyTrigger]] =
    val words = config.getValue(path).valueType match
      case ConfigValueType.LIST => config.getList(path).asScala.toList.map(_.unwrapped().toString)
      case _                    => config.getValue(path).unwrapped().toString.split(",").toList
    val parsed = words.map(_.trim).filter(_.nonEmpty).map(HotkeyTrigger.parse)
    Option.when(parsed.forall(_.isDefined))(parsed.flatten)
