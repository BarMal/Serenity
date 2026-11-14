package com.serenity.ui.skia

/** The Skiko build on the class path, for the places that must name it without naming Skiko themselves. */
object SkikoVersion:

  def current: Option[String] =
    Option(classOf[org.jetbrains.skiko.SkiaLayer].getPackage).flatMap(p => Option(p.getImplementationVersion))
