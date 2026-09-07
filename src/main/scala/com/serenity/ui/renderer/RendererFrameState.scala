package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

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
  * force a full redraw the same blunt way `Renderer`'s retired `ChromeKey` did by including these fields directly in
  * its own structural comparison.
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
  *
  * Weak keys: the base image pool recycles a small number of images, and an image the pool drops must not be held alive
  * by this cache. Access is synchronised because the render thread is not guaranteed to be a single thread across the
  * app's lifetime.
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

/** Per-frame caches `Renderer` consults across the whole module: the prepared render plan a cursor-only redraw can
  * reuse, the accumulated repaint damage a persisting surface hasn't drawn yet, and the previous frame's floating-panel
  * rects/pane snapshots later frames diff against. Split out of `Renderer` itself so the mutable, synchronized
  * bookkeeping lives in one place regardless of which render entry point or frame-planning step reads or updates it.
  */
object RendererFrameState:

  /** Frame state below is kept per surface, keyed by the same [[SurfaceContentIdentity]] the damage accumulator uses:
    * what one surface drew last frame says nothing about what another one preserves, and a single slot shared by every
    * surface made whichever surface painted most recently the "previous frame" for all of them. A surface with no
    * persistence key preserves nothing between frames, so it has no previous frame to remember and none is kept for it
    * -- it redraws in full, which is what a non-persisting surface does anyway.
    *
    * Weak keys and synchronised access for the reasons [[bufferDamage]] documents: the base image pool recycles its
    * images, and the render thread is not guaranteed to be one thread for the app's lifetime.
    */
  private val preparedScenes = new java.util.WeakHashMap[SurfaceContentIdentity, PreparedScene]()

  def preparedSceneFor(surface: RenderSurface): Option[PreparedScene] =
    surface.persistentContentKey.flatMap(key => preparedScenes.synchronized(Option(preparedScenes.get(key))))

  def rememberPreparedScene(surface: RenderSurface, scene: PreparedScene): Unit =
    surface.persistentContentKey.foreach(key => preparedScenes.synchronized { val _ = preparedScenes.put(key, scene) })

  /** Screen-pixel rects the floating panels (`renderFloatingPanels`'s `overlays`) occupied on the *previous* frame a
    * surface painted, keyed by surface id within it. `planFrame`/`dirtyRowsFor` read this before `renderFloatingPanels`
    * runs for the current frame, so it always reflects the frame before the one being planned -- exactly what's needed
    * to find the pane rows a moved/resized/closed panel vacated (see `dirtyRowsFor`'s doc comment). Updated at the end
    * of `renderFloatingPanels` once this frame's rects are known, ready for the next frame's `planFrame` call.
    */
  private val previousFloatingSurfaceRects =
    new java.util.WeakHashMap[SurfaceContentIdentity, Map[SurfaceId, PixelRect]]()

  def previousFloatingSurfaceRectsFor(key: SurfaceContentIdentity): Map[SurfaceId, PixelRect] =
    previousFloatingSurfaceRects.synchronized(Option(previousFloatingSurfaceRects.get(key))).getOrElse(Map.empty)

  def rememberFloatingSurfaceRects(surface: RenderSurface, rects: Map[SurfaceId, PixelRect]): Unit =
    surface.persistentContentKey.foreach { key =>
      previousFloatingSurfaceRects.synchronized { val _ = previousFloatingSurfaceRects.put(key, rects) }
    }

  /** Each pane's [[TextLayoutSnapshot]] as of the *previous* frame planned for a surface, keyed by pane id within it --
    * `dirtyRowsFor` reads this (via `planFrame`, which always runs before this state is updated for the frame being
    * planned) to catch rows whose content shifted because an *upstream* paragraph's edit changed how many rows it wraps
    * into, not just rows whose own buffer line `Damage` names directly (see [[DirtyLineDiff]]'s doc comment). Updated
    * right after `planFrame` returns for a frame, from that same frame's `renderPlan.snapshots`, so the next frame's
    * `planFrame` call sees this frame's snapshots as "previous" -- the same before/after timing
    * [[previousFloatingSurfaceRects]] documents above, except the snapshot itself (not a later paint step) is already
    * known by the time `planFrame` runs, so there's no need to wait for painting to record it.
    */
  private val previousSnapshots = new java.util.WeakHashMap[SurfaceContentIdentity, Map[PaneId, TextLayoutSnapshot]]()

  def previousSnapshotsFor(key: SurfaceContentIdentity): Map[PaneId, TextLayoutSnapshot] =
    previousSnapshots.synchronized(Option(previousSnapshots.get(key))).getOrElse(Map.empty)

  def rememberSnapshots(surface: RenderSurface, snapshots: Map[PaneId, TextLayoutSnapshot]): Unit =
    surface.persistentContentKey.foreach { key =>
      previousSnapshots.synchronized { val _ = previousSnapshots.put(key, snapshots) }
    }

  /** Drops `key`'s remembered previous-frame snapshots and floating-panel rects -- used by
    * [[RendererFramePlanner.forgetPreservedContent]] when a surface's preserved pixels are no longer valid, since both
    * are reuse promises about pixels that surface no longer holds.
    */
  def forgetPreviousFrameState(key: SurfaceContentIdentity): Unit =
    previousSnapshots.synchronized { val _ = previousSnapshots.remove(key) }
    previousFloatingSurfaceRects.synchronized { val _ = previousFloatingSurfaceRects.remove(key) }

  val bufferDamage    = new java.util.WeakHashMap[SurfaceContentIdentity, Damage]()
  val bufferDrawState = new java.util.WeakHashMap[SurfaceContentIdentity, DrawState]()

  /** Which screen each tracked buffer identity was last drawn for, so [[accumulateBufferDamage]] only folds a frame's
    * damage into buffers that actually belong to the same screen -- e.g. the same `SwingWindow`'s own two pooled
    * images. Without this, every buffer identity ever tracked shares one process-wide map with no notion of which ones
    * are actually part of the same image pool: harmless in production (there is only ever one real window, so "every
    * tracked identity" and "every identity in this window's pool" are the same set), but any other concurrently running
    * render session -- another window, or another independent `render` call in the same process -- would otherwise have
    * its damage silently mixed into this one's, and vice versa.
    */
  val bufferScreen = new java.util.WeakHashMap[SurfaceContentIdentity, ScreenIdentity]()

  /** Distinct from [[bufferDamage]] because base images alternate: the pixels a surface preserves come from two frames
    * ago, while the screen shows the last one published.
    */
  val screenDamage  = new java.util.WeakHashMap[ScreenIdentity, Damage]()
  val screenPaneIds = new java.util.WeakHashMap[ScreenIdentity, Set[PaneId]]()

  /** Keyed by the [[RenderSurface]] a frame was painted onto, exactly like [[bufferScreen]] above and for the same
    * reason: a single JVM-wide slot would let one surface's cached modal image leak into another surface's frame --
    * harmless in production (there is only ever one real window) but a real hazard for any other concurrently running
    * render session in the same process, tests included, since `Renderer` is a singleton every suite shares. A
    * `WeakHashMap` (rather than the `AtomicReference` this used to be) lets a surface's entry disappear once nothing
    * else references it, instead of pinning every `RenderSurface` a process ever rendered to for its whole lifetime.
    */
  val modalLayerBuffers = new java.util.WeakHashMap[RenderSurface, CachedModalLayer]()

  /** Keyed by [[RenderSurface]] first and [[SurfaceId]] second, for the same cross-surface-leak reason as
    * [[modalLayerBuffers]] -- a bare `Map[SurfaceId, CachedPanelLayer]` shared process-wide let any two independently
    * rendered surfaces that happen to reuse the same `SurfaceId` (unremarkable: tests across many specs all use
    * `SurfaceId("outline")`) stomp on each other's cached panel image.
    */
  val panelLayerBuffers = new java.util.WeakHashMap[RenderSurface, Map[SurfaceId, CachedPanelLayer]]()

  /** Folds `damage` into every buffer identity already being tracked for `output`'s screen (every identity, if there is
    * no output to scope by), via [[DamageAccumulator.accumulateBuffers]] -- called unconditionally on every frame,
    * regardless of which identity (if any) this specific frame draws into, so a pixel buffer sitting idle through
    * several frames still accrues what each of them changed.
    */
  def accumulateBufferDamage(output: Option[FrameOutput], damage: Damage): Unit =
    bufferDamage.synchronized {
      val tracked = mapAsScala(bufferDamage)
      val scoped = output match
        case None => tracked
        case Some(value) =>
          tracked.filter { case (key, _) => Option(bufferScreen.get(key)).forall(_ == value.screenToken) }
      val updated = DamageAccumulator.accumulateBuffers(scoped, damage)
      updated.foreach { case (key, value) => val _ = bufferDamage.put(key, value) }
    }

  /** Reports and resets what has accumulated for `persistenceKey` since it was last drawn into, via
    * [[DamageAccumulator.observeBufferDraw]] -- `Damage.Everything` if this is the first time this identity has been
    * seen, since an untracked identity's pixels cannot be trusted at all, not merely assumed unchanged. Also records
    * `output`'s screen as this identity's owner, so future [[accumulateBufferDamage]] calls scope correctly.
    */
  def drainBufferDamage(output: Option[FrameOutput], persistenceKey: SurfaceContentIdentity): Damage =
    bufferDamage.synchronized {
      output.foreach(value => bufferScreen.put(persistenceKey, value.screenToken))
      val wasTracked          = bufferDamage.containsKey(persistenceKey)
      val (observed, updated) = DamageAccumulator.observeBufferDraw(mapAsScala(bufferDamage), persistenceKey)
      updated.foreach { case (key, value) => val _ = bufferDamage.put(key, value) }
      if wasTracked then observed else Damage.Everything
    }

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
    bufferDrawState.synchronized {
      val next     = DrawState(paneIds, inputs)
      val previous = Option(bufferDrawState.get(persistenceKey))
      val _        = bufferDrawState.put(persistenceKey, next)
      !previous.contains(next)
    }

  /** Folds `damage` into the screen's own accumulated total for `output`'s screen, via
    * [[DamageAccumulator.accumulateScreen]]. Distinct from [[accumulateBufferDamage]] because exactly one buffer is
    * ever on screen at a time, so there is only ever one running total to fold into, not one per tracked identity.
    */
  def accumulateScreenDamage(output: Option[FrameOutput], damage: Damage): Unit =
    output.foreach { value =>
      screenDamage.synchronized {
        val current = Option(screenDamage.get(value.screenToken)).getOrElse(Damage.Nothing)
        val _       = screenDamage.put(value.screenToken, DamageAccumulator.accumulateScreen(current, damage))
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
        screenDamage.synchronized {
          val wasTracked        = screenDamage.containsKey(value.screenToken)
          val current           = Option(screenDamage.get(value.screenToken)).getOrElse(Damage.Nothing)
          val (observed, reset) = DamageAccumulator.observeScreenPublish(current)
          val _                 = screenDamage.put(value.screenToken, reset)
          if wasTracked then observed else Damage.Everything
        }

  /** As [[drawStateChanged]], but for the screen's own published pane set -- distinct because the screen shows the
    * previous frame while a surface's own preserved pixels come from the one before that (see [[screenDamage]]).
    */
  def screenPaneIdsChanged(output: Option[FrameOutput], paneIds: Set[PaneId]): Boolean =
    output match
      case None => true
      case Some(value) =>
        screenPaneIds.synchronized {
          val previous = Option(screenPaneIds.get(value.screenToken))
          val _        = screenPaneIds.put(value.screenToken, paneIds)
          !previous.contains(paneIds)
        }

  private def mapAsScala(map: java.util.Map[SurfaceContentIdentity, Damage]): Map[SurfaceContentIdentity, Damage] =
    map.asScala.toMap

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
