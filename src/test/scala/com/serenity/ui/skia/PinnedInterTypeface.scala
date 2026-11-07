package com.serenity.ui.skia

import org.jetbrains.skia.{Data, FontMgr, Typeface}

/** Skia's answer for the family "Inter" in tests that pin [[com.serenity.ui.layout.BundledInterFont]]: that font is
  * loaded into AWT only, so Skia would otherwise draw whichever Inter the machine has installed, or a fallback face
  * where it has none, and the runs a frame splits into would differ by machine.
  */
object PinnedInterTypeface:

  private val Family   = "Inter"
  private val Resource = "/fonts/Inter-Regular-Subset.ttf"

  private lazy val subset: Typeface =
    val stream = getClass.getResourceAsStream(Resource)
    try
      val bytes = stream.readAllBytes()
      FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0)
    finally stream.close()

  /** The subset for Inter, `elsewhere` for every other family. */
  def resolver(elsewhere: SkiaTypefaces.TypefaceKey => Typeface): SkiaTypefaces.TypefaceKey => Typeface =
    key => if key.family.equalsIgnoreCase(Family) then subset else elsewhere(key)

  val system: SkiaTypefaces.TypefaceKey => Typeface = resolver(SkiaTypefaces.typefaceFor)
