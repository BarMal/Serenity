package com.serenity.ui.skia

import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The painter's side of [[RenderCaches]] ownership: the Skia text caches belong to the [[RenderCaches]] that renders,
  * so a row drawn through one owner is shaped in that owner's cache and in nobody else's.
  */
class SkiaTextCachesOwnershipSpec extends AnyFlatSpec with Matchers:

  private val font     = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val metrics  = CellMetrics.fromFont(font)
  private val widthPx  = 900
  private val heightPx = 40
  // A character from U+0300 up is what sends text through the shaper; plain ASCII is drawn without shaping.
  private val line = "owned → by one RenderCaches, distinct to the ownership spec"

  private def drawn(owner: RenderCaches): Unit =
    val frame = SkiaFrames.newFrame(widthPx, heightPx)
    val surface =
      SkiaFrames.surfaceOver(frame, metrics, font, widthPx, heightPx, 1.0, persists = false, owner, None, _ => ())
    surface.text.drawShapedRunPx(0.0f, 0, widthPx.toFloat, metrics.lineHeight, metrics.ascent, line, 0, line.length)

  "Two RenderCaches" should "own distinct Skia text caches" in {
    val (a, b) = (RenderCaches.create(), RenderCaches.create())

    (SkiaTextCaches.of(a.backendCaches) `ne` SkiaTextCaches.of(b.backendCaches)) shouldBe true
    (SkiaTextCaches.of(a.backendCaches).shapedText `ne` SkiaTextCaches.of(b.backendCaches).shapedText) shouldBe true
  }

  "One RenderCaches" should "give every ask the same Skia text caches" in {
    val owner = RenderCaches.create()

    (SkiaTextCaches.of(owner.backendCaches) eq SkiaTextCaches.of(owner.backendCaches)) shouldBe true
  }

  "Drawing a row through an owned surface" should "shape it in that owner's cache and no other's" in {
    val (owner, other) = (RenderCaches.create(), RenderCaches.create())

    drawn(owner)

    SkiaTextCaches.of(owner.backendCaches).shapedText.shapings should be > 0L
    SkiaTextCaches.heldBy(other.backendCaches) shouldBe false
  }
