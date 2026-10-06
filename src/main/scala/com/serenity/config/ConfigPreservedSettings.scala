package com.serenity.config

import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}

private[config] object ConfigPreservedSettings:

  /** The rendered settings followed by every setting in the file this version does not recognise, as found.
    *
    * Without this a key from a newer build, a typo the user means to fix, or a setting for a plugin is silently dropped
    * by the first save. Comments the user wrote are not kept; the backup is where they survive.
    */
  def apply(rendered: String, existing: Config): Either[String, String] =
    val kept = existing
      .entrySet()
      .asScala
      .toList
      .filter { entry =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        !ConfigKeySchema.isKnownKey(key) && !RemovedConfigKeys.isRemoved(key)
      }
      .sortBy(_.getKey)
      .map(entry => s"${entry.getKey} = ${entry.getValue.render(ConfigRenderOptions.concise())}")
    if kept.isEmpty then Right(rendered)
    else
      val text =
        (rendered :: "# Settings this version of Serenity does not recognise, kept exactly as found." :: kept)
          .mkString("\n") + "\n"
      Try(ConfigFactory.parseString(text)).toEither.map(_ => text).left.map { error =>
        s"The settings in the existing file that Serenity does not recognise would not survive the save: ${error.getMessage}"
      }
