package com.serenity.app

import java.nio.file.Path

final case class LaunchOptions(openPath: Option[Path] = None)

object LaunchOptions:

  def parse(args: List[String]): LaunchOptions =
    args match
      case "--open" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)))
      case "--file" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)))
      case path :: _ if !path.startsWith("-") =>
        LaunchOptions(openPath = Some(Path.of(path)))
      case _ =>
        LaunchOptions()
