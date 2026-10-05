package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.MockRenderSurface
import com.serenity.state.models.{BufferId, Damage, PaneId, SurfaceId}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the [[RendererFrameState]] caches that #1411 moved off `java.util.WeakHashMap` + `synchronized` onto a
  * `Ref[IO, ...]`-backed, capacity-bounded store: the public per-surface/per-screen accessors behave exactly as before
  * (round trip, "first time" reporting, forgetting), and the bounded eviction that now stands in for `WeakHashMap`'s
  * GC-driven eviction is itself exercised -- an evicted identity must fall back to the same "never tracked" behaviour
  * an identity this module has genuinely never seen falls back to, since that is what makes eviction safe rather than
  * merely convenient.
  */
class RendererFrameStateSpec extends AnyFlatSpec with Matchers:

  // Issue #1677: an instance private to this spec, not the JVM-wide singleton `RendererFrameState` used to be -- other
  // specs running concurrently can no longer observe or reset this capacity out from under a test here.
  private val frameState = RendererFrameState(64)

  private def surface(persistent: Boolean = true): MockRenderSurface = new MockRenderSurface(10, 5, persistent)

  private def frameOutput(screen: AnyRef): FrameOutput =
    FrameOutput(ScreenIdentity(screen), new AtomicReference[Option[com.serenity.ui.layout.PixelRect]](None))

  private def image(): RenderImage = RenderImage.fromAwt(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))

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
    val s   = surface()
    val key = s.persistentContentKey.get
    frameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(1), Set(0)))
    frameState.drainBufferDamage(None, key) shouldBe Damage.Everything
  }

  it should "accumulate damage across frames and reset it on drain" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    frameState.drainBufferDamage(None, key) // start tracking
    frameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(2), Set(0)))
    frameState.accumulateBufferDamage(None, Damage.BufferRows(BufferId(2), Set(1)))
    frameState.drainBufferDamage(None, key) shouldBe Damage.BufferRows(BufferId(2), Set(0, 1))
    frameState.drainBufferDamage(None, key) shouldBe Damage.Nothing
  }

  it should "scope accumulation to identities tracked for the same screen" in {
    val s1      = surface()
    val s2      = surface()
    val k1      = s1.persistentContentKey.get
    val k2      = s2.persistentContentKey.get
    val outputA = frameOutput(new Object)
    val outputB = frameOutput(new Object)
    frameState.drainBufferDamage(Some(outputA), k1)
    frameState.drainBufferDamage(Some(outputB), k2)

    frameState.accumulateBufferDamage(Some(outputA), Damage.BufferRows(BufferId(3), Set(0)))

    frameState.drainBufferDamage(Some(outputA), k1) shouldBe Damage.BufferRows(BufferId(3), Set(0))
    frameState.drainBufferDamage(Some(outputB), k2) shouldBe Damage.Nothing
  }

  "drawStateChanged" should "report a change the first time a persistence key is seen" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe true
  }

  it should "report no change when paneIds and inputs are identical to the last call" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe false
  }

  it should "report a change when the pane set differs" in {
    val s   = surface()
    val key = s.persistentContentKey.get
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    frameState.drawStateChanged(key, Set(PaneId(1), PaneId(2)), someInputs) shouldBe true
  }

  "accumulateScreenDamage / drainScreenDamage" should "report Everything with no output" in {
    frameState.drainScreenDamage(None) shouldBe Damage.Everything
  }

  it should "report Everything the first time a screen is drained, then accumulate and reset" in {
    val output = frameOutput(new Object)
    frameState.drainScreenDamage(Some(output)) shouldBe Damage.Everything
    frameState.accumulateScreenDamage(Some(output), Damage.BufferRows(BufferId(4), Set(0)))
    frameState.drainScreenDamage(Some(output)) shouldBe Damage.BufferRows(BufferId(4), Set(0))
    frameState.drainScreenDamage(Some(output)) shouldBe Damage.Nothing
  }

  "screenPaneIdsChanged" should "report true with no output" in {
    frameState.screenPaneIdsChanged(None, Set(PaneId(1))) shouldBe true
  }

  it should "report false only when the pane set matches the last call for that screen" in {
    val output = frameOutput(new Object)
    frameState.screenPaneIdsChanged(Some(output), Set(PaneId(1))) shouldBe true
    frameState.screenPaneIdsChanged(Some(output), Set(PaneId(1))) shouldBe false
    frameState.screenPaneIdsChanged(Some(output), Set(PaneId(2))) shouldBe true
  }

  "modal layer buffer caching" should "round-trip and forget" in {
    val s     = surface()
    val layer = CachedModalLayer(image(), 80, 24, cursorVisible = true)
    frameState.cachedModalLayerFor(s) shouldBe None
    frameState.rememberModalLayerBuffer(s, layer)
    frameState.cachedModalLayerFor(s) shouldBe Some(layer)
    frameState.forgetModalLayerBuffer(s)
    frameState.cachedModalLayerFor(s) shouldBe None
  }

  "panel layer buffer caching" should "round-trip per surfaceId and prune inactive ones" in {
    val s      = surface()
    val outer  = SurfaceId("outline")
    val inner  = SurfaceId("preview")
    val layer1 = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(0, 0, 1, 1))
    val layer2 = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(1, 1, 1, 1))

    frameState.cachedPanelLayersFor(s) shouldBe Map.empty
    frameState.rememberPanelLayer(s, outer, layer1)
    frameState.rememberPanelLayer(s, inner, layer2)
    frameState.cachedPanelLayersFor(s) shouldBe Map(outer -> layer1, inner -> layer2)

    frameState.pruneStalePanelLayers(s, Set(inner))
    frameState.cachedPanelLayersFor(s) shouldBe Map(inner -> layer2)
  }

  "layer buffer caching" should "share one entry between successive surfaces painting the same window" in {
    // A GUI frame is a fresh surface every time (#1798); keying by the surface itself made every frame a new entry.
    val window      = new Object
    val firstFrame  = new OwnedMockSurface(window)
    val secondFrame = new OwnedMockSurface(window)
    val modal       = CachedModalLayer(image(), 80, 24, cursorVisible = true)
    val panel = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(0, 0, 1, 1))
    val state = RendererFrameState(64)

    state.rememberModalLayerBuffer(firstFrame, modal)
    state.rememberPanelLayer(firstFrame, SurfaceId("outline"), panel)

    state.cachedModalLayerFor(secondFrame) shouldBe Some(modal)
    state.cachedPanelLayersFor(secondFrame) shouldBe Map(SurfaceId("outline") -> panel)
    state.cachedModalLayerFor(new OwnedMockSurface(new Object)) shouldBe None
  }

  it should "forget both modal and panel layers for a window at once" in {
    val s     = surface()
    val panel = CachedPanelLayer(image(), 80, 24, cursorVisible = true, com.serenity.ui.layout.LayoutRect(0, 0, 1, 1))
    frameState.rememberModalLayerBuffer(s, CachedModalLayer(image(), 80, 24, cursorVisible = true))
    frameState.rememberPanelLayer(s, SurfaceId("outline"), panel)

    frameState.forgetLayerBuffers(s)

    frameState.cachedModalLayerFor(s) shouldBe None
    frameState.cachedPanelLayersFor(s) shouldBe Map.empty
  }

  "forgetPreviousFrameState" should "drop remembered floating rects for the key" in {
    val s     = surface()
    val key   = s.persistentContentKey.get
    val rects = Map(SurfaceId("a") -> com.serenity.ui.layout.PixelRect(0, 0, 1, 1))
    frameState.rememberFloatingSurfaceRects(s, rects)
    frameState.previousFloatingSurfaceRectsFor(key) shouldBe rects

    frameState.forgetPreviousFrameState(key)

    frameState.previousFloatingSurfaceRectsFor(key) shouldBe Map.empty
  }

  "forgetBufferState / forgetScreenState" should "make a previously tracked identity look untracked again" in {
    val s      = surface()
    val key    = s.persistentContentKey.get
    val output = frameOutput(new Object)
    frameState.drainBufferDamage(Some(output), key)
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs)
    frameState.forgetBufferState(key)
    frameState.drainBufferDamage(None, key) shouldBe Damage.Everything
    frameState.drawStateChanged(key, Set(PaneId(1)), someInputs) shouldBe true

    frameState.drainScreenDamage(Some(output))
    frameState.screenPaneIdsChanged(Some(output), Set(PaneId(1)))
    frameState.forgetScreenState(output.screenToken)
    frameState.drainScreenDamage(Some(output)) shouldBe Damage.Everything
  }

  /** [[frameState.cacheCapacity]] is this spec's own instance-scoped state (issue #1677): a test that reconfigures it
    * must still restore the previous value afterward, or a later test in this same `frameState` instance relying on the
    * default would silently see a different bound than it assumed.
    */
  private def withCacheCapacity[A](capacity: Int)(test: => A): A =
    val previous = frameState.currentCacheCapacity
    frameState.configureCacheCapacity(capacity)
    try test
    finally frameState.configureCacheCapacity(previous)

  "the bounded per-cache store" should "evict the least recently written entry once capacity is exceeded" in
    withCacheCapacity(64) {
      // One more than the configured per-cache capacity: every screen identity but the very first gets a fresh
      // drain (first-drain semantics == "never tracked"), and the first one must go back to reporting Everything
      // once it's pushed out, exactly like a WeakHashMap entry whose key nothing else reaches anymore.
      val screens = List.fill(frameState.currentCacheCapacity + 1)(frameOutput(new Object))

      screens.foreach { output =>
        frameState.drainScreenDamage(Some(output)) // first touch of each: establishes tracking
      }

      val evicted = screens.head
      frameState.drainScreenDamage(Some(evicted)) shouldBe Damage.Everything
    }

  "configureCacheCapacity" should "clamp to AppConfig's configured bounds" in
    withCacheCapacity(64) {
      frameState.configureCacheCapacity(Int.MaxValue)
      frameState.currentCacheCapacity shouldBe com.serenity.config.AppConfig.MaxRendererFrameStateCacheCapacity

      frameState.configureCacheCapacity(-100)
      frameState.currentCacheCapacity shouldBe com.serenity.config.AppConfig.MinRendererFrameStateCacheCapacity
    }

  it should "shrink the bound live, evicting down to the new capacity on the next write" in
    withCacheCapacity(8) {
      val screens = List.fill(8)(frameOutput(new Object))
      screens.foreach(output => frameState.drainScreenDamage(Some(output)))

      // Shrinking alone doesn't retroactively evict -- only a write re-checks the bound, same as the retired
      // WeakHashMap only ever shed entries lazily, never eagerly, on a capacity/GC change.
      frameState.configureCacheCapacity(4)
      val trigger = frameOutput(new Object)
      frameState.drainScreenDamage(Some(trigger)) // one write past the new capacity

      val oldestStillTracked = screens.drop(screens.size - 3).map(_.screenToken)
      screens.map(_.screenToken).filterNot(oldestStillTracked.contains).foreach { evictedToken =>
        frameState.drainScreenDamage(Some(FrameOutput(evictedToken, trigger.repaintRegion))) shouldBe
          Damage.Everything
      }
    }

  it should "grow the bound live, so entries beyond the old capacity stop evicting each other" in
    withCacheCapacity(4) {
      val screens = List.fill(4)(frameOutput(new Object))
      screens.foreach(output => frameState.drainScreenDamage(Some(output)))

      frameState.configureCacheCapacity(8)
      val extra = List.fill(4)(frameOutput(new Object))
      extra.foreach(output => frameState.drainScreenDamage(Some(output)))

      // All 8 fit under the raised capacity, so the original 4 (which would have been evicted under the old
      // capacity of 4) are still tracked.
      screens.foreach(output => frameState.drainScreenDamage(Some(output)) shouldNot be(Damage.Everything))
    }

  private class OwnedMockSurface(window: AnyRef) extends MockRenderSurface(10, 5, true):
    override def layerCacheOwner: ScreenIdentity = ScreenIdentity(window)
