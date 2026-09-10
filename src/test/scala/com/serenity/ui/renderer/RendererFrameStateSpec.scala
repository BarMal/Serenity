package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import com.serenity.MockRenderSurface
import com.serenity.state.models.{BufferId, Damage, PaneId, SurfaceId}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the [[RendererFrameState]] caches that #1411 moved off `java.util.WeakHashMap` + `synchronized` onto a
  * `Ref[IO, ...]`-backed, capacity-bounded store: the public per-surface/per-screen accessors behave exactly as
  * before (round trip, "first time" reporting, forgetting), and the bounded eviction that now stands in for
  * `WeakHashMap`'s GC-driven eviction is itself exercised -- an evicted identity must fall back to the same
  * "never tracked" behaviour an identity this module has genuinely never seen falls back to, since that is what
  * makes eviction safe rather than merely convenient.
  */
class RendererFrameStateSpec extends AnyFlatSpec with Matchers:

  private def surface(persistent: Boolean = true): MockRenderSurface = new MockRenderSurface(10, 5, persistent)

  private def frameOutput(screen: AnyRef): FrameOutput =
    FrameOutput(ScreenIdentity(screen), new AtomicReference[Option[com.serenity.ui.layout.PixelRect]](None))

  private def image(): BufferedImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)

  private val someInputs = RenderInputs(
    ViewportSize(80, 24),
    new Font(Font.MONOSPACED, Font.PLAIN, 12),
    new Font(Font.MONOSPACED, Font.PLAIN, 12),
    new Font(Font.MONOSPACED, Font.PLAIN, 12),
    CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
    CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
    cursorVisible = true,
    cursorColor = None
  )

  "accumulateBufferDamage / drainBufferDamage" should "report Everything the first time an identity is drained" in {
    val s = surface()
    val key = s.persistentContentKey.get
    RendererFrameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(1), Set(0)))
    RendererFrameState.drainBufferDamage(None, key) shouldBe Damage.Everything
  }

  it should "accumulate damage across frames and reset it on drain" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    RendererFrameState.drainBufferDamage(None, key) // start tracking
    RendererFrameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(2), Set(0)))
    RendererFrameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(2), Set(1)))
    RendererFrameState.drainBufferDamage(None, key) shouldBe Damage.BufferRows(BufferId(2), Set(0, 1))
    RendererFrameState.drainBufferDamage(None, key) shouldBe Damage.Nothing
  }

  it should "scope accumulation to identities tracked for the same screen" in {
    val s1 = surface()
    val s2 = surface()
    val k1 = s1.persistentContentKey.get
    val k2 = s2.persistentContentKey.get
    val outputA = frameOutput(new Object)
    val outputB = frameOutput(new Object)
    RendererFrameState.drainBufferDamage(Some(outputA), k1)
    RendererFrameState.drainBufferDamage(Some(outputB), k2)

    RendererFrameState.accumulateBufferDamage(Some(outputA), Damage.BufferRows(BufferId(3), Set(0)))

    RendererFrameState.drainBufferDamage(Some(outputA), k1) shouldBe Damage.BufferRows(BufferId(3), Set(0))
    RendererFrameState.drainBufferDamage(Some(outputB), k2) shouldBe Damage.Nothing
  }

  "drawStateChanged" should "report a change the first time a persistence key is seen" in {
    val s = surface()
    val key = s.persistentContentKey.get
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe true
  }

  it should "report no change when paneIds and inputs are identical to the last call" in {
    val s = surface()
    val key = s.persistentContentKey.get
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe false
  }

  it should "report a change when the pane set differs" in {
    val s = surface()
    val key = s.persistentContentKey.get
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    RendererFrameState.drawStateChanged(key, Set(PaneId(1), PaneId(2)), someInputs) shouldBe true
  }

  "accumulateScreenDamage / drainScreenDamage" should "report Everything with no output" in {
    RendererFrameState.drainScreenDamage(None) shouldBe Damage.Everything
  }

  it should "report Everything the first time a screen is drained, then accumulate and reset" in {
    val output = frameOutput(new Object)
    RendererFrameState.drainScreenDamage(Some(output)) shouldBe Damage.Everything
    RendererFrameState.accumulateScreenDamage(Some(output), Damage.BufferRows(BufferId(4), Set(0)))
    RendererFrameState.drainScreenDamage(Some(output)) shouldBe Damage.BufferRows(BufferId(4), Set(0))
    RendererFrameState.drainScreenDamage(Some(output)) shouldBe Damage.Nothing
  }

  "screenPaneIdsChanged" should "report true with no output" in {
    RendererFrameState.screenPaneIdsChanged(None, Set(PaneId(1))) shouldBe true
  }

  it should "report false only when the pane set matches the last call for that screen" in {
    val output = frameOutput(new Object)
    RendererFrameState.screenPaneIdsChanged(Some(output), Set(PaneId(1))) shouldBe true
    RendererFrameState.screenPaneIdsChanged(Some(output), Set(PaneId(1))) shouldBe false
    RendererFrameState.screenPaneIdsChanged(Some(output), Set(PaneId(2))) shouldBe true
  }

  "modal layer buffer caching" should "round-trip and forget" in {
    val s     = surface()
    val layer = CachedModalLayer(image(), 80, 24, cursorVisible = true)
    RendererFrameState.cachedModalLayerFor(s) shouldBe None
    RendererFrameState.rememberModalLayerBuffer(s, layer)
    RendererFrameState.cachedModalLayerFor(s) shouldBe Some(layer)
    RendererFrameState.forgetModalLayerBuffer(s)
    RendererFrameState.cachedModalLayerFor(s) shouldBe None
  }

  "panel layer buffer caching" should "round-trip per surfaceId and prune inactive ones" in {
    val s      = surface()
    val outer  = SurfaceId("outline")
    val inner  = SurfaceId("preview")
    val layer1 = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(0, 0, 1, 1))
    val layer2 = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(1, 1, 1, 1))

    RendererFrameState.cachedPanelLayersFor(s) shouldBe Map.empty
    RendererFrameState.rememberPanelLayer(s, outer, layer1)
    RendererFrameState.rememberPanelLayer(s, inner, layer2)
    RendererFrameState.cachedPanelLayersFor(s) shouldBe Map(outer -> layer1, inner -> layer2)

    RendererFrameState.pruneStalePanelLayers(s, Set(inner))
    RendererFrameState.cachedPanelLayersFor(s) shouldBe Map(inner -> layer2)
  }

  "forgetPreviousFrameState" should "drop remembered floating rects for the key" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    val rects = Map(SurfaceId("a") -> com.serenity.ui.layout.PixelRect(0, 0, 1, 1))
    RendererFrameState.rememberFloatingSurfaceRects(s, rects)
    RendererFrameState.previousFloatingSurfaceRectsFor(key) shouldBe rects

    RendererFrameState.forgetPreviousFrameState(key)

    RendererFrameState.previousFloatingSurfaceRectsFor(key) shouldBe Map.empty
  }

  "forgetBufferState / forgetScreenState" should "make a previously tracked identity look untracked again" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    val output = frameOutput(new Object)
    RendererFrameState.drainBufferDamage(Some(output), key)
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    RendererFrameState.forgetBufferState(key)
    RendererFrameState.drainBufferDamage(None, key) shouldBe Damage.Everything
    RendererFrameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe true

    RendererFrameState.drainScreenDamage(Some(output))
    RendererFrameState.screenPaneIdsChanged(Some(output), Set(PaneId(1)))
    RendererFrameState.forgetScreenState(output.screenToken)
    RendererFrameState.drainScreenDamage(Some(output)) shouldBe Damage.Everything
  }

  "the bounded per-cache store" should "evict the least recently written entry once capacity is exceeded" in {
    // One more than RendererFrameState's documented per-cache capacity: every screen identity but the very first
    // gets a fresh drain (first-drain semantics == "never tracked"), and the first one must go back to reporting
    // Everything once it's pushed out, exactly like a WeakHashMap entry whose key nothing else reaches anymore.
    val screens = List.fill(RendererFrameState.PerCacheCapacity + 1)(frameOutput(new Object))

    screens.foreach { output =>
      RendererFrameState.drainScreenDamage(Some(output)) // first touch of each: establishes tracking
    }

    val evicted = screens.head
    RendererFrameState.drainScreenDamage(Some(evicted)) shouldBe Damage.Everything
  }
