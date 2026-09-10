package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Identity of the screen a frame publishes to -- a newtype over the backing canvas/surface's own reference identity,
  * for the same reason [[SurfaceContentIdentity]] wraps a pixel buffer's.
  */
opaque type ScreenIdentity = AnyRef

object ScreenIdentity:
  def apply(value: AnyRef): ScreenIdentity = value

/** Where a frame goes: the screen it will be shown on, and the region sink that screen's repaint should honour. */
final case class FrameOutput(screenToken: ScreenIdentity, repaintRegion: AtomicReference[Option[PixelRect]])

/** The render parameters that shape a frame but are not part of `AppState`, so `DamageProducer` -- which only diffs
  * `AppState` -- has no way to see them change: the window's pixel size, the three fonts, and the metrics/overrides a
  * caller derives from them. Tracked per persistence key so a mismatch against the last frame drawn with that key can
  * force a full redraw the same blunt way the retired `ChromeKey` did by including these fields directly in its own
  * structural comparison.
  */
final case class RenderInputs(
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color]
)

/** Per-persistence-key bookkeeping this module remembers across frames, replacing the retired `ChromeKey`/
  * `FrameRecord`/`PaneContentKey`/`PaneRowKey` reconstruct-and-diff machinery. Nothing here holds a structural snapshot
  * of a frame to diff against -- staleness is `Damage`, reported by the code that made a change, accumulated across
  * however many frames a pixel buffer or the screen sits unvisited via [[DamageAccumulator]]. What is kept is only
  * enough to know when that accumulation cannot be trusted on its own: the set of panes actually drawn (a pane can
  * appear or disappear -- e.g. an empty buffer gaining content -- without the layout reference changing, which is
  * `paneChromeDamage`'s usual signal for that), and the non-`AppState` [[RenderInputs]] above.
  */
final case class DrawState(paneIds: Set[PaneId], inputs: RenderInputs)

/** What [[RendererFramePlanner.paintModalLayer]] cached the last time it painted the modal layer into its own buffer
  * (#1100 stage 2): the flushed image, and the frame shape/cursor-blink state it was painted for. Reused only when a
  * later frame's modal layer is clean (see [[DamageProducer]]'s `modalOnlyContentChange`) *and* still matches both
  * fields -- a viewport resize or a blink toggle invalidates the cache even though neither is `AppState`, so a stale
  * buffer is never composited over a frame it no longer matches. One slot per surface, not keyed by frame-buffer
  * identity, because exactly one modal is ever open at a time on a given surface and the cached image is just a source
  * for `drawImage`, independent of which of the (possibly pooled) frame buffers it gets composited onto.
  */
final case class CachedModalLayer(
    image: BufferedImage,
    viewportWidth: Int,
    viewportHeight: Int,
    cursorVisible: Boolean
)

/** What [[RendererFramePlanner.paintPanelLayer]] cached the last time it painted a given pinned/expanded/floating panel
  * into its own buffer (#1100 stage 3): the [[CachedModalLayer]] pattern generalised to every panel kind that reads
  * pixels back off the frame surface via `blurRegion`, keyed by [[SurfaceId]] rather than held as a single slot, since
  * -- unlike the modal -- more than one of these panels can be on screen at once. `frameRect` is remembered alongside
  * viewport shape and cursor-blink state because a panel's own rect can shift (another panel appearing/disappearing
  * reflows pinned layout) without the panel's own `UiSurface` fields changing, which [[Damage.Surface]] narrowing alone
  * would not catch.
  */
final case class CachedPanelLayer(
    image: BufferedImage,
    viewportWidth: Int,
    viewportHeight: Int,
    cursorVisible: Boolean,
    frameRect: LayoutRect
)

/** The render-plan cache entry [[RendererFramePlanner.prepareScene]] produces and cursor-only entry points reuse when
  * nothing that would invalidate it has changed.
  */
final case class PreparedScene(
    scene: UiSceneSnapshot,
    renderPlan: EditorPaneRenderPlan,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    viewportSize: ViewportSize
):

  def matches(
    candidateScene: UiSceneSnapshot,
    candidateCodeFont: java.awt.Font,
    candidateTextFont: java.awt.Font,
    candidateUiFont: java.awt.Font,
    candidateCellMetrics: CellMetrics,
    candidateUiMetrics: CellMetrics,
    candidateViewportSize: ViewportSize
  ): Boolean =
    (scene eq candidateScene) &&
      codeFont == candidateCodeFont &&
      textFont == candidateTextFont &&
      uiFont == candidateUiFont &&
      cellMetrics == candidateCellMetrics &&
      uiMetrics == candidateUiMetrics &&
      viewportSize == candidateViewportSize

/** A `Ref[IO, Map[K, V]]`-backed cache bounded to [[RendererFrameState.PerCacheCapacity]] most-recently-written
  * entries, standing in for `java.util.WeakHashMap`'s GC-driven eviction (see that constant's doc comment for why a
  * recency bound is an equivalent, and simpler, way to keep these process-wide caches from growing without limit).
  * `Ref.modify`'s CAS retry loop replaces the manual `synchronized` blocks this module used to need for the same
  * "render thread isn't guaranteed to be one thread" reason; every accessor below runs its `IO` synchronously
  * (`unsafeRunSync`) so callers keep the module's long-standing synchronous API.
  */
final private class BoundedRefCache[K, V](capacity: Int):
  private case class Contents(entries: Map[K, V], order: Vector[K])

  private val state: Ref[IO, Contents] = Ref.unsafe(Contents(Map.empty, Vector.empty))

  private def bounded(contents: Contents): Contents =
    if contents.order.size <= capacity then contents
    else
      val dropCount = contents.order.size - capacity
      val dropped   = contents.order.take(dropCount)
      Contents(contents.entries -- dropped, contents.order.drop(dropCount))

  def get(key: K): Option[V] =
    state.get.map(_.entries.get(key)).unsafeRunSync()

  def snapshot: Map[K, V] =
    state.get.map(_.entries).unsafeRunSync()

  def put(key: K, value: V): Unit =
    state
      .update(c => bounded(Contents(c.entries.updated(key, value), c.order.filterNot(_ == key) :+ key)))
      .unsafeRunSync()

  def remove(key: K): Unit =
    state.update(c => Contents(c.entries - key, c.order.filterNot(_ == key))).unsafeRunSync()

  /** Rewrites every entry at once (e.g. folding damage into several tracked identities together) rather than one key
    * at a time.
    */
  def replaceAll(f: Map[K, V] => Map[K, V]): Unit =
    modify(entries => (f(entries), ()))

  /** As [[cats.effect.Ref.modify]]: transforms the whole map and reports a value computed alongside that transform,
    * atomically.
    */
  def modify[A](f: Map[K, V] => (Map[K, V], A)): A =
    state
      .modify { c =>
        val (nextEntries, result) = f(c.entries)
        val nextOrder             = c.order.filter(nextEntries.contains) ++ nextEntries.keySet.diff(c.order.toSet).toVector
        (bounded(Contents(nextEntries, nextOrder)), result)
      }
      .unsafeRunSync()

/** Per-frame caches consulted across the whole module: the prepared render plan a cursor-only redraw can reuse, the
  * accumulated repaint damage a persisting surface hasn't drawn yet, and the previous frame's floating-panel rects/pane
  * snapshots later frames diff against. All the module's mutable bookkeeping lives here, behind [[BoundedRefCache]],
  * in one place, regardless of which render entry point or frame-planning step reads or updates it.
  */
object RendererFrameState:

  /** Per-cache capacity for every [[BoundedRefCache]] below. These caches are process-wide singletons keyed by object
    * identity (`SurfaceContentIdentity`, `ScreenIdentity`, `RenderSurface`) with no notion of when their key is done
    * being useful -- the retired `WeakHashMap`s handled that by letting an entry disappear once nothing else in the
    * app referenced its key, which also kept a recycled image-pool identity or a long-lived test run's surfaces from
    * pinning cache entries forever. A `Ref`-backed `Map` cannot observe reachability the way a weak key can, so this
    * bounds growth the other safe way instead: capping each cache to its most recently *written* entries and evicting
    * the rest. Eviction is safe (not just convenient) because every reader here already treats an untracked identity
    * exactly like one that has never been seen -- `drainBufferDamage`/`drainScreenDamage` report `Damage.Everything`,
    * `drawStateChanged`/`screenPaneIdsChanged` report a change, and a cached layer image simply isn't reused. So an
    * evicted-but-still-live surface costs one extra full redraw the next time it's touched, not incorrect output.
    * 64 mirrors the bound [[com.serenity.state.manager.AuthoritativeUiScene]]'s own `prepared` cache already uses for
    * the same "no real bound, but must not grow forever" reason, comfortably above the handful of surfaces/screens a
    * single window (or a single test) ever has live at once.
    */
  val PerCacheCapacity: Int = 64

  /** Frame state below is kept per surface, keyed by the same [[SurfaceContentIdentity]] the damage accumulator uses:
    * what one surface drew last frame says nothing about what another one preserves, and a single slot shared by every
    * surface made whichever surface painted most recently the "previous frame" for all of them. A surface with no
    * persistence key preserves nothing between frames, so it has no previous frame to remember and none is kept for it
    * -- it redraws in full, which is what a non-persisting surface does anyway.
    */
  private val preparedScenes = new BoundedRefCache[SurfaceContentIdentity, PreparedScene](PerCacheCapacity)

  def preparedSceneFor(surface: RenderSurface): Option[PreparedScene] =
    surface.persistentContentKey.flatMap(preparedScenes.get)

  def rememberPreparedScene(surface: RenderSurface, scene: PreparedScene): Unit =
    surface.persistentContentKey.foreach(key => preparedScenes.put(key, scene))

  /** Screen-pixel rects the floating panels (`renderFloatingPanels`'s `overlays`) occupied on the *previous* frame a
    * surface painted, keyed by surface id within it. `planFrame`/`dirtyRowsFor` read this before `renderFloatingPanels`
    * runs for the current frame, so it always reflects the frame before the one being planned -- exactly what's needed
    * to find the pane rows a moved/resized/closed panel vacated (see `dirtyRowsFor`'s doc comment). Updated at the end
    * of `renderFloatingPanels` once this frame's rects are known, ready for the next frame's `planFrame` call.
    */
  private val previousFloatingSurfaceRects =
    new BoundedRefCache[SurfaceContentIdentity, Map[SurfaceId, PixelRect]](PerCacheCapacity)

  def previousFloatingSurfaceRectsFor(key: SurfaceContentIdentity): Map[SurfaceId, PixelRect] =
    previousFloatingSurfaceRects.get(key).getOrElse(Map.empty)

  def rememberFloatingSurfaceRects(surface: RenderSurface, rects: Map[SurfaceId, PixelRect]): Unit =
    surface.persistentContentKey.foreach(key => previousFloatingSurfaceRects.put(key, rects))

  /** Each pane's [[TextLayoutSnapshot]] as of the *previous* frame planned for a surface, keyed by pane id within it --
    * `dirtyRowsFor` reads this (via `planFrame`, which always runs before this state is updated for the frame being
    * planned) to catch rows whose content shifted because an *upstream* paragraph's edit changed how many rows it wraps
    * into, not just rows whose own buffer line `Damage` names directly (see [[DirtyLineDiff]]'s doc comment). Updated
    * right after `planFrame` returns for a frame, from that same frame's `renderPlan.snapshots`, so the next frame's
    * `planFrame` call sees this frame's snapshots as "previous" -- the same before/after timing
    * [[previousFloatingSurfaceRects]] documents above, except the snapshot itself (not a later paint step) is already
    * known by the time `planFrame` runs, so there's no need to wait for painting to record it.
    */
  private val previousSnapshots =
    new BoundedRefCache[SurfaceContentIdentity, Map[PaneId, TextLayoutSnapshot]](PerCacheCapacity)

  def previousSnapshotsFor(key: SurfaceContentIdentity): Map[PaneId, TextLayoutSnapshot] =
    previousSnapshots.get(key).getOrElse(Map.empty)

  def rememberSnapshots(surface: RenderSurface, snapshots: Map[PaneId, TextLayoutSnapshot]): Unit =
    surface.persistentContentKey.foreach(key => previousSnapshots.put(key, snapshots))

  /** Drops `key`'s remembered previous-frame snapshots and floating-panel rects -- used by
    * [[RendererFramePlanner.forgetPreservedContent]] when a surface's preserved pixels are no longer valid, since both
    * are reuse promises about pixels that surface no longer holds.
    */
  def forgetPreviousFrameState(key: SurfaceContentIdentity): Unit =
    previousSnapshots.remove(key)
    previousFloatingSurfaceRects.remove(key)

  private val bufferDamage    = new BoundedRefCache[SurfaceContentIdentity, Damage](PerCacheCapacity)
  private val bufferDrawState = new BoundedRefCache[SurfaceContentIdentity, DrawState](PerCacheCapacity)

  /** Which screen each tracked buffer identity was last drawn for, so [[accumulateBufferDamage]] only folds a frame's
    * damage into buffers that actually belong to the same screen -- e.g. the same `SwingWindow`'s own two pooled
    * images. Without this, every buffer identity ever tracked shares one process-wide map with no notion of which ones
    * are actually part of the same image pool: harmless in production (there is only ever one real window, so "every
    * tracked identity" and "every identity in this window's pool" are the same set), but any other concurrently running
    * render session -- another window, or another independent `render` call in the same process -- would otherwise have
    * its damage silently mixed into this one's, and vice versa.
    */
  private val bufferScreen = new BoundedRefCache[SurfaceContentIdentity, ScreenIdentity](PerCacheCapacity)

  /** Distinct from [[bufferDamage]] because base images alternate: the pixels a surface preserves come from two frames
    * ago, while the screen shows the last one published.
    */
  private val screenDamage  = new BoundedRefCache[ScreenIdentity, Damage](PerCacheCapacity)
  private val screenPaneIds = new BoundedRefCache[ScreenIdentity, Set[PaneId]](PerCacheCapacity)

  /** Keyed by the [[RenderSurface]] a frame was painted onto, exactly like [[bufferScreen]] above and for the same
    * reason: a single JVM-wide slot would let one surface's cached modal image leak into another surface's frame --
    * harmless in production (there is only ever one real window) but a real hazard for any other concurrently running
    * render session in the same process, tests included, since these caches are process-wide singletons every suite
    * shares.
    */
  private val modalLayerBuffers = new BoundedRefCache[RenderSurface, CachedModalLayer](PerCacheCapacity)

  def cachedModalLayerFor(surface: RenderSurface): Option[CachedModalLayer] = modalLayerBuffers.get(surface)

  def rememberModalLayerBuffer(surface: RenderSurface, layer: CachedModalLayer): Unit =
    modalLayerBuffers.put(surface, layer)

  def forgetModalLayerBuffer(surface: RenderSurface): Unit = modalLayerBuffers.remove(surface)

  /** Keyed by [[RenderSurface]] first and [[SurfaceId]] second, for the same cross-surface-leak reason as
    * [[modalLayerBuffers]] -- a bare `Map[SurfaceId, CachedPanelLayer]` shared process-wide let any two independently
    * rendered surfaces that happen to reuse the same `SurfaceId` (unremarkable: tests across many specs all use
    * `SurfaceId("outline")`) stomp on each other's cached panel image.
    */
  private val panelLayerBuffers = new BoundedRefCache[RenderSurface, Map[SurfaceId, CachedPanelLayer]](PerCacheCapacity)

  def cachedPanelLayersFor(surface: RenderSurface): Map[SurfaceId, CachedPanelLayer] =
    panelLayerBuffers.get(surface).getOrElse(Map.empty)

  def rememberPanelLayer(surface: RenderSurface, surfaceId: SurfaceId, layer: CachedPanelLayer): Unit =
    panelLayerBuffers.replaceAll { tracked =>
      val current = tracked.getOrElse(surface, Map.empty[SurfaceId, CachedPanelLayer])
      tracked.updated(surface, current.updated(surfaceId, layer))
    }

  /** Drop cached panel buffers for surfaces no longer on screen this frame, scoped to `surface`'s own entry -- a
    * dismissed panel's cache would otherwise sit in that inner `Map[SurfaceId, _]` forever ([[SurfaceId]] is a plain
    * value, not an object this module can bound the lifetime of any other way).
    */
  def pruneStalePanelLayers(surface: RenderSurface, activeIds: Set[SurfaceId]): Unit =
    panelLayerBuffers.replaceAll { tracked =>
      val current = tracked.getOrElse(surface, Map.empty[SurfaceId, CachedPanelLayer])
      tracked.updated(surface, current.filter { case (id, _) => activeIds.contains(id) })
    }

  /** Drops every buffer-scoped cache entry for `key` -- used by [[RendererFramePlanner.forgetPreservedContent]]
    * alongside [[forgetPreviousFrameState]] when a surface's preserved pixels are no longer valid.
    */
  def forgetBufferState(key: SurfaceContentIdentity): Unit =
    bufferDamage.remove(key)
    bufferScreen.remove(key)
    bufferDrawState.remove(key)

  /** Drops every screen-scoped cache entry for `screenToken`, the [[forgetBufferState]] counterpart for the screen
    * rather than a preserving surface.
    */
  def forgetScreenState(screenToken: ScreenIdentity): Unit =
    screenDamage.remove(screenToken)
    screenPaneIds.remove(screenToken)

  /** Folds `damage` into every buffer identity already being tracked for `output`'s screen (every identity, if there is
    * no output to scope by), via [[DamageAccumulator.accumulateBuffers]] -- called unconditionally on every frame,
    * regardless of which identity (if any) this specific frame draws into, so a pixel buffer sitting idle through
    * several frames still accrues what each of them changed.
    */
  def accumulateBufferDamage(output: Option[FrameOutput], damage: Damage): Unit =
    val screenOwners = bufferScreen.snapshot
    bufferDamage.replaceAll { tracked =>
      val scoped = output match
        case None => tracked
        case Some(value) =>
          tracked.filter { case (key, _) => screenOwners.get(key).forall(_ == value.screenToken) }
      val updated = DamageAccumulator.accumulateBuffers(scoped, damage)
      tracked ++ updated
    }

  /** Reports and resets what has accumulated for `persistenceKey` since it was last drawn into, via
    * [[DamageAccumulator.observeBufferDraw]] -- `Damage.Everything` if this is the first time this identity has been
    * seen, since an untracked identity's pixels cannot be trusted at all, not merely assumed unchanged. Also records
    * `output`'s screen as this identity's owner, so future [[accumulateBufferDamage]] calls scope correctly.
    */
  def drainBufferDamage(output: Option[FrameOutput], persistenceKey: SurfaceContentIdentity): Damage =
    output.foreach(value => bufferScreen.put(persistenceKey, value.screenToken))
    val wasTracked = bufferDamage.get(persistenceKey).isDefined
    val observed = bufferDamage.modify { tracked =>
      val (obs, updated) = DamageAccumulator.observeBufferDraw(tracked, persistenceKey)
      (updated, obs)
    }
    if wasTracked then observed else Damage.Everything

  /** Records `paneIds`/`inputs` as this identity's current draw state and reports whether either differs from what was
    * remembered -- a pane appearing or disappearing without a layout-reference change (an empty buffer gaining
    * content), or a render parameter `DamageProducer` cannot see because it isn't part of `AppState` (a window resize,
    * a font swap), both mean the accumulated buffer damage alone cannot be trusted for this frame.
    */
  def drawStateChanged(
    persistenceKey: SurfaceContentIdentity,
    paneIds: Set[PaneId],
    inputs: RenderInputs
  ): Boolean =
    val next = DrawState(paneIds, inputs)
    bufferDrawState.modify { tracked =>
      val previous = tracked.get(persistenceKey)
      (tracked.updated(persistenceKey, next), !previous.contains(next))
    }

  /** Folds `damage` into the screen's own accumulated total for `output`'s screen, via
    * [[DamageAccumulator.accumulateScreen]]. Distinct from [[accumulateBufferDamage]] because exactly one buffer is
    * ever on screen at a time, so there is only ever one running total to fold into, not one per tracked identity.
    */
  def accumulateScreenDamage(output: Option[FrameOutput], damage: Damage): Unit =
    output.foreach { value =>
      screenDamage.replaceAll { tracked =>
        val current = tracked.getOrElse(value.screenToken, Damage.Nothing)
        tracked.updated(value.screenToken, DamageAccumulator.accumulateScreen(current, damage))
      }
    }

  /** Reports and resets what has accumulated for the screen since it was last published to, via
    * [[DamageAccumulator.observeScreenPublish]] -- `Damage.Everything` when there is no output to bound a repaint
    * against, or this is the first publish this screen has ever been tracked for.
    */
  def drainScreenDamage(output: Option[FrameOutput]): Damage =
    output match
      case None => Damage.Everything
      case Some(value) =>
        val wasTracked = screenDamage.get(value.screenToken).isDefined
        val observed = screenDamage.modify { tracked =>
          val current       = tracked.getOrElse(value.screenToken, Damage.Nothing)
          val (obs, reset) = DamageAccumulator.observeScreenPublish(current)
          (tracked.updated(value.screenToken, reset), obs)
        }
        if wasTracked then observed else Damage.Everything

  /** As [[drawStateChanged]], but for the screen's own published pane set -- distinct because the screen shows the
    * previous frame while a surface's own preserved pixels come from the one before that (see [[screenDamage]]).
    */
  def screenPaneIdsChanged(output: Option[FrameOutput], paneIds: Set[PaneId]): Boolean =
    output match
      case None => true
      case Some(value) =>
        screenPaneIds.modify { tracked =>
          val previous = tracked.get(value.screenToken)
          (tracked.updated(value.screenToken, paneIds), !previous.contains(paneIds))
        }

  /** Logical pixels map one-to-one onto canvas component pixels: the canvas scales the frame image back to its own
    * logical size when it paints, so a region measured against the frame is already in component coordinates.
    */
  def toAwtRectangle(rect: PixelRect): java.awt.Rectangle =
    new java.awt.Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)

  def renderInputsFor(context: RenderContext, viewportSize: ViewportSize): RenderInputs =
    RenderInputs(
      viewportSize,
      context.codeFont,
      context.textFont,
      context.uiFont,
      context.cellMetrics,
      context.uiMetrics,
      context.cursorVisible,
      context.cursorColorOverride
    )
