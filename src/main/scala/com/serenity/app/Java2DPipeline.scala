package com.serenity.app

import cats.effect.IO

object Java2DPipeline:

  private val WindowsSafeDefaults: Map[String, String] =
    Map(
      "sun.java2d.d3d"    -> "false",
      "sun.java2d.opengl" -> "false"
    )

  def installSafeDefaults(): IO[Unit] =
    IO {
      missingDefaults(
        osName = System.getProperty("os.name", ""),
        existingProperties = key => Option(System.getProperty(key))
      ).foreach {
        case (key, value) =>
          System.setProperty(key, value)
      }
    }

  private[serenity] def missingDefaults(
    osName: String,
    existingProperties: String => Option[String]
  ): Map[String, String] =
    if isWindows(osName) then WindowsSafeDefaults.filter { case (key, _) => existingProperties(key).isEmpty }
    else Map.empty

  private def isWindows(osName: String): Boolean =
    osName.toLowerCase(java.util.Locale.ROOT).contains("win")
