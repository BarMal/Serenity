package com.serenity.app

import java.awt.GraphicsEnvironment
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.serenity.app.RendererBenchmarkResult.Key

/** The key a measurement is kept under (#2618), from what the coordinator can read about the machine. What only a
  * window can report, the GL renderer, is `unknown`: a driver change alone does not mark a result stale.
  */
object MachineKey:

  private val Unknown = "unknown"

  def of(
    properties: Map[String, String],
    env: Map[String, String],
    display: Option[String],
    skiko: Option[String],
    onMains: Option[Boolean]
  ): Key =
    val os = List(properties.get("os.name"), properties.get("os.version")).flatten match
      case Nil   => Unknown
      case parts => parts.mkString(" ")
    val toolkit =
      if env.get("XDG_SESSION_TYPE").contains("wayland") || env.get("WAYLAND_DISPLAY").exists(_.nonEmpty) then "wayland"
      else env.get("XDG_SESSION_TYPE").filter(_.nonEmpty).getOrElse(Unknown)
    Key(
      glRenderer = Unknown,
      os = os,
      toolkit = toolkit,
      display = display.getOrElse(Unknown),
      jvm = properties.getOrElse("java.vm.version", Unknown),
      skiko = skiko.getOrElse(Unknown),
      powerState = onMains.fold(Unknown)(if _ then "ac" else "battery")
    )

  def current: Key =
    of(
      System.getProperties.asScala.toMap,
      sys.env,
      display = screen,
      skiko =
        Option(classOf[org.jetbrains.skiko.SkiaLayer].getPackage).flatMap(p => Option(p.getImplementationVersion)),
      onMains = onMains
    )

  private def screen: Option[String] =
    Try {
      val configuration = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
      val bounds        = configuration.getBounds
      s"${bounds.width}x${bounds.height}@${configuration.getDefaultTransform.getScaleX}"
    }.toOption

  /** Whether any mains supply reports online, read from sysfs where there is one. */
  private def onMains: Option[Boolean] =
    val supplies = Path.of("/sys/class/power_supply")
    Try {
      Files
        .list(supplies)
        .iterator()
        .asScala
        .toList
        .filter(path => Files.readString(path.resolve("type")).trim == "Mains")
        .map(path => Files.readString(path.resolve("online")).trim == "1")
    }.toOption.filter(_.nonEmpty).map(_.exists(identity))
