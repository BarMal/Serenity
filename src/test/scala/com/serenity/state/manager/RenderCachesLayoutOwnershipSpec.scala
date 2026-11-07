package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The layout side of [[RenderCaches]] ownership, with no surface involved: laying a line out through one owner leaves
  * it shaped in that owner's cache and in nobody else's. The painter's side is in
  * [[com.serenity.ui.skia.SkiaTextCachesOwnershipSpec]].
  */
class RenderCachesLayoutOwnershipSpec extends AnyFlatSpec with Matchers:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
  private val widthPx = 900
  private val line    = "val owned = items.map(_ + 1).filter(_ >= 2) // a => b != c, distinct to the ownership spec"

  "Two RenderCaches" should "own distinct renderer text caches" in {
    val (a, b) = (RenderCaches.create(), RenderCaches.create())

    (a.shapedText `ne` b.shapedText) shouldBe true
    (a.styledFonts `ne` b.styledFonts) shouldBe true
    (a.labelSprites `ne` b.labelSprites) shouldBe true
    (a.measuredAdvances `ne` b.measuredAdvances) shouldBe true
  }

  "Laying a ligature line out through a RenderCaches" should "shape it in that owner's cache and no other's" in {
    val (owner, other) = (RenderCaches.create(), RenderCaches.create())
    val frc            = TextLayoutSnapshot.defaultFontRenderContext()
    owner.shapedText.holds(line, font, frc) shouldBe false

    val rows = TextLayoutSnapshot.boundedVisualLinesForText(line, 0, widthPx, font, frc, wrapCache = owner.wrappedLines)

    rows.map(_.text) shouldBe Vector(line)
    owner.shapedText.holds(line, font, frc) shouldBe true
    other.shapedText.holds(line, font, frc) shouldBe false
  }

  "A RenderCaches' measured advances" should "shape through the owner's cache, not a shared one" in {
    val (owner, other) = (RenderCaches.create(), RenderCaches.create())
    val frc            = TextLayoutSnapshot.defaultFontRenderContext()

    val advance = owner.measuredAdvances.advanceOf(line, font, frc)

    advance should be > 0.0f
    owner.measuredAdvances.measurements shouldBe 1L
    other.measuredAdvances.measurements shouldBe 0L
    owner.shapedText.holds(line, font, frc) shouldBe true
    other.shapedText.holds(line, font, frc) shouldBe false
  }

  "A RenderCaches' surface text caches" should "be the owner's own instances" in {
    val owner = RenderCaches.create()

    (owner.surfaceText.shapedText eq owner.shapedText) shouldBe true
    (owner.surfaceText.styledFonts eq owner.styledFonts) shouldBe true
    (owner.surfaceText.labelSprites eq owner.labelSprites) shouldBe true
  }
