package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.TestMeasure
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.TextLayoutSnapshot
import com.serenity.ui.skia.SkiaTextCaches
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The layout side of [[RenderCaches]] ownership, with no surface involved: laying a line out through one owner
  * measures it with that owner's Skia text caches and with nobody else's. The painter's side is in
  * [[com.serenity.ui.skia.SkiaTextCachesOwnershipSpec]].
  */
class RenderCachesLayoutOwnershipSpec extends AnyFlatSpec with Matchers:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
  private val widthPx = 900
  private val line    = "val owned = items.map(_ + 1).filter(_ >= 2) // a => b != c, distinct to the ownership spec"

  "Two RenderCaches" should "own distinct renderer text caches" in {
    val (a, b) = (TestMeasure.renderCaches(), TestMeasure.renderCaches())

    (a.styledFonts `ne` b.styledFonts) shouldBe true
    (a.labelSprites `ne` b.labelSprites) shouldBe true
    (a.measuredAdvances `ne` b.measuredAdvances) shouldBe true
  }

  "Laying a ligature line out through a RenderCaches" should "measure it with that owner's text caches and no other's" in {
    val (owner, other) = (TestMeasure.renderCaches(), TestMeasure.renderCaches())
    SkiaTextCaches.heldBy(owner.backendCaches) shouldBe false

    val rows = TextLayoutSnapshot.boundedVisualLinesForText(line, 0, widthPx, font, wrapCache = owner.wrappedLines)

    rows.map(_.text) shouldBe Vector(line)
    SkiaTextCaches.of(owner.backendCaches).fonts.size should be > 0
    SkiaTextCaches.heldBy(other.backendCaches) shouldBe false
  }

  "A RenderCaches' measured advances" should "measure through the owner's text caches, not a shared one" in {
    val (owner, other) = (TestMeasure.renderCaches(), TestMeasure.renderCaches())

    val advance = owner.measuredAdvances.advanceOf(line, font)

    advance should be > 0.0f
    owner.measuredAdvances.measurements shouldBe 1L
    other.measuredAdvances.measurements shouldBe 0L
    SkiaTextCaches.heldBy(owner.backendCaches) shouldBe true
    SkiaTextCaches.heldBy(other.backendCaches) shouldBe false
  }

  "A RenderCaches' surface text caches" should "be the owner's own instances" in {
    val owner = TestMeasure.renderCaches()

    (owner.surfaceText.styledFonts eq owner.styledFonts) shouldBe true
    (owner.surfaceText.labelSprites eq owner.labelSprites) shouldBe true
  }
