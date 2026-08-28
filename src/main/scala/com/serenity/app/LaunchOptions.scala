package com.serenity.app

import java.nio.file.Path

final case class LaunchOptions(openPath: Option[Path] = None, eco: Boolean = false)

object LaunchOptions:

  def parse(args: List[String]): LaunchOptions =
    val eco = args.contains("--eco")
    // --eco is a bare flag with no value, so it's stripped before the positional/--open/--file matching below --
    // that logic only looks at the head of the list and shouldn't have to account for --eco's position.
    args.filterNot(_ == "--eco") match
      case "--open" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco)
      case "--file" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco)
      case path :: _ if !path.startsWith("-") =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco)
      case _ =>
        LaunchOptions(eco = eco)
