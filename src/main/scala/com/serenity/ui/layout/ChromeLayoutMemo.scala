package com.serenity.ui.layout

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.serenity.state.models.AppState

/** Remembers the last few chrome layouts, so one keystroke's scene, accessibility, hit-testing and toolbar paths share
  * a single `calculateLayoutWithUI` instead of each laying out the whole window again (#1919, #1946).
  *
  * The layout reads the active buffer only to anchor floating surfaces at the caret, and every buffer's line count only
  * for the gutter's width, so an edit that leaves both alone keeps the memo current. State is only ever replaced, never
  * mutated, so an unchanged reference is an unchanged input; a value-equal but rebuilt input costs a recompute, never a
  * stale layout.
  */
final class ChromeLayoutMemo(capacity: Int = ChromeLayoutMemo.DefaultCapacity):

  private val entries      = new AtomicReference[List[ChromeLayoutMemo.Entry]](Nil)
  private val computations = new AtomicInteger(0)

  /** How many layouts were computed rather than remembered. */
  def computationCount: Int = computations.get

  def layoutFor(state: AppState, viewportSize: ViewportSize, spacerPercentage: Double)(
    compute: => CalculatedLayout
  ): CalculatedLayout =
    val key = ChromeLayoutMemo.Key.of(state, viewportSize, spacerPercentage)
    entries.get.find(_.key.matches(key)) match
      case Some(entry) => entry.layout
      case None =>
        val layout = compute
        computations.incrementAndGet()
        entries.updateAndGet(current => (ChromeLayoutMemo.Entry(key, layout) :: current).take(capacity))
        layout

object ChromeLayoutMemo:

  val DefaultCapacity = 2

  final private case class Entry(key: Key, layout: CalculatedLayout)

  /** `references` are compared by identity; `values` by equality. */
  final private case class Key(
      viewportSize: ViewportSize,
      spacerPercentage: Double,
      gutterCounterWidth: Int,
      values: List[Any],
      references: List[AnyRef]
  ):

    def matches(other: Key): Boolean =
      viewportSize == other.viewportSize && spacerPercentage == other.spacerPercentage &&
        gutterCounterWidth == other.gutterCounterWidth && values == other.values &&
        references.corresponds(other.references)(_ eq _)

  private object Key:

    def of(state: AppState, viewportSize: ViewportSize, spacerPercentage: Double): Key =
      val persisted         = state.persisted
      val runtime           = state.runtime
      val statusLineSurface = state.floatingStatusLineSurface
      val anchorsToCaret    = statusLineSurface.isDefined || state.floatingSurfaces.nonEmpty
      Key(
        viewportSize = viewportSize,
        spacerPercentage = spacerPercentage,
        gutterCounterWidth = LayoutEngine.lineNumberCounterWidth(state),
        values = List(
          statusLineSurface,
          state.chrome,
          persisted.bufferOrder.size,
          runtime.pointerGesture.cursorPeekResolvedAnchor
        ),
        references = List(
          persisted.config,
          persisted.layout,
          persisted.focus,
          runtime.uiSurfaces,
          runtime.capabilities
        ) ++ (if anchorsToCaret then state.activeBuffer.toList else Nil)
      )
