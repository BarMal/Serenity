package com.serenity.ui.skia

import org.jetbrains.skia.{Data, FontMgr, Typeface}

/** A [[SkiaFontCache]] for fonts the tests load through AWT alone: the Inter subset is not a runtime font, so no host
  * has it installed for Skia to find. Every other family resolves as in production.
  */
object TestBundledTypefaces:

  private val InterResource = "/fonts/Inter-Regular-Subset.ttf"

  private lazy val inter: Option[Typeface] =
    Option(getClass.getResourceAsStream(InterResource)).flatMap { stream =>
      try
        val bytes = stream.readAllBytes()
        Option(FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0))
      finally stream.close()
    }

  def cacheWithInter(): SkiaFontCache =
    new SkiaFontCache(resolveTypeface =
      key => Option.when(key.family == "Inter")(inter).flatten.getOrElse(SkiaTypefaces.typefaceFor(key))
    )
