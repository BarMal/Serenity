package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What #2660 item 2, step D3, removes from main: the AWT text stack that Skia's measurer and caches replaced. Each
  * test is one change of that step and names what must no longer be there.
  */
final class AwtTextStackGoneSpec extends AnyFlatSpec with Matchers:

  private val Main = Paths.get("src", "main", "scala")

  private def mainFiles: List[Path] =
    Using.resource(Files.walk(Main))(stream =>
      stream.iterator().asScala.filter(path => path.toString.endsWith(".scala")).toList.sorted
    )

  private def filesMentioning(pattern: String): List[String] =
    val regex = pattern.r
    mainFiles
      .filter(path => regex.findFirstIn(Files.readString(path)).isDefined)
      .map(path => Main.relativize(path).toString)

  "Main" should "hold no AWT shaping stack: no shaped-text cache, shaped stretch, glyph-advance table or font probes" in {
    filesMentioning("""\b(ShapedTextCache|ShapedStretch|GlyphAdvance|FontProbes)\b""") shouldBe Nil
  }

  it should "hold no label-sprite cache, which rasterised text through AWT and which nothing asked" in {
    filesMentioning("""\bLabelSprites\b""") shouldBe Nil
  }

  it should "ask AWT for no cell metrics: an owner with no measurer is on the cell grid" in {
    filesMentioning("""CellMetrics\.fromFont|def fromFont\(font: java\.awt\.Font\)|awtChAdvancePx""") shouldBe Nil
  }

  it should "read no AWT line metrics: the rules drawn with text come from the font's Skia metrics" in {
    filesMentioning("""getLineMetrics|AwtFontReconciliation""") shouldBe Nil
  }

  it should "hold no first-use settling of AWT fonts: nothing measures an AWT font" in {
    filesMentioning("""\bFontFirstUse\b""") shouldBe Nil
  }

  it should "name no FontRenderContext: nothing measures text through AWT's context" in {
    filesMentioning("""\bFontRenderContext\b|defaultFontRenderContext|fontRenderContext""") shouldBe Nil
  }

  it should "hold no Java2D code nothing calls: a Graphics2D font helper, a Path2D rounded outline, an AffineTransform label anchor" in {
    filesMentioning("""\bRoundedPath\b|\bdrawingIn\b|transform: AffineTransform""") shouldBe Nil
  }

  it should "ask AWT whether a font has a glyph nowhere: coverage is read from Skia typefaces, so layout and paint agree" in {
    filesMentioning("""\bAwtGlyphCoverage\b|\.canDisplay\(""") shouldBe Nil
  }
