package com.serenity.ui.skia

import org.jetbrains.skia.{Data, FontMgr}

/** For specs only: makes a font from the classpath visible to Skia's font caches. */
object SkiaTestFonts:

  /** Registers the font at `resource` and returns its family name; registering it again changes nothing. */
  private[serenity] def register(resource: String): String =
    val stream = getClass.getResourceAsStream(resource)
    require(stream != null, s"no font resource $resource")
    val bytes =
      try stream.readAllBytes()
      finally stream.close()
    val typeface = FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0)
    require(typeface != null, s"Skia cannot read the font $resource")
    SkiaTypefaces.register(typeface)
    typeface.getFamilyName
