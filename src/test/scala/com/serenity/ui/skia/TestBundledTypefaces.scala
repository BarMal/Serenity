package com.serenity.ui.skia

import com.serenity.ui.renderer.BackendCaches
import org.jetbrains.skia.{Data, FontMgr, Typeface}

/** A [[SkiaFontCache]] for fonts the tests load through AWT alone: the Inter subset and the two test fonts of the
  * fallback specs are not runtime fonts, so no host has them installed for Skia to find. Every other family resolves as
  * in production.
  */
object TestBundledTypefaces:

  private val Fixtures = List(
    "/fonts/Inter-Regular-Subset.ttf",
    "/fonts/SerenityFallbackTest.ttf",
    "/fonts/SerenityProseTest.ttf"
  )

  private def load(resource: String): Option[Typeface] =
    Option(getClass.getResourceAsStream(resource)).flatMap { stream =>
      try
        val bytes = stream.readAllBytes()
        Option(FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0))
      finally stream.close()
    }

  private lazy val fixtures: Map[String, Typeface] =
    Fixtures.flatMap(load).map(typeface => typeface.getFamilyName.toLowerCase -> typeface).toMap

  /** One holder for every test of the JVM, so the faces are opened once. */
  private val typefaces = new SharedTypefaces(resolveWithFixtures)

  private def resolveWithFixtures(key: SkiaTypefaces.TypefaceKey): Typeface =
    fixtures.getOrElse(key.family.toLowerCase, SkiaTypefaces.typefaceFor(key))

  def cacheWithInter(): SkiaFontCache = new SkiaFontCache(resolveTypeface = typefaces.typefaceFor)

  /** Gives `owner` the Skia text caches its surfaces and measurer use with the fixture fonts resolvable; must come
    * before anything draws or measures for it.
    */
  def installInter(owner: BackendCaches): Unit = SkiaTextCaches.withTypefaces(owner, typefaces.typefaceFor)
