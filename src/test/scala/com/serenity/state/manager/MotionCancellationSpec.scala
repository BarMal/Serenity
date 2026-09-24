package com.serenity.state.manager

import java.awt.Color

import com.serenity.animation.{
  AnimatedCell,
  AnimationOwner,
  AnimationState,
  CharacterKey,
  EasingCurve,
  TransitionDirection,
  Tween
}
import com.serenity.command.CommandRunner
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility, MotionFamily}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.PanelStateReducer
import com.serenity.ui.layout.{LayoutRect, PanelContent, PanelPosition, PixelPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pure motion cancellation (#1697 wave 2): which in-flight motion a config change cancels, and what cancelling it
  * leaves behind -- plain values in and out, each result checked against `AppStateValidation` because these used to be
  * unvalidated `stateRef.update` writes (#1183).
  */
class MotionCancellationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId     = BufferId(0)
  private val paletteId    = SurfaceId("palette")
  private val paletteGhost = SurfaceId("palette-ghost")
  private val panelGhost   = SurfaceId("panel-ghost")

  private def tween(steps: Int): Tween[Color] =
    Tween(new Color(0, 0, 0), new Color(255, 255, 255), EasingCurve.Linear, steps)

  private def fadeFor(phase: SurfacePhase): SurfaceAnimationState =
    SurfaceAnimationState(
      phase = phase,
      animationState = AnimationState(
        Map(CharacterKey(0, 0) -> AnimatedCell(None, Some(tween(4)), Some(tween(4)), AnimationOwner.UiTransitions))
      ),
      overlayHeight = 1,
      bufferFadeLength = 0,
      phaseTick = 0
    )

  private def geometry: PanelGeometryState =
    PanelGeometryState(Tween(LayoutRect(0, 0, 20, 10), LayoutRect(0, 0, 0, 10), EasingCurve.Linear, 4))

  /** Every cancellable motion family in flight at once, on an otherwise ordinary (valid) initial state. */
  private val inFlight: AppState =
    val base = AppState.initial(AppConfig.default)
    val cursor = Cursor(
      CursorPosition(0, 0),
      Some(CursorPosition(0, 0)),
      glide = Some(Tween(PixelPoint(0, 0), PixelPoint(20, 0), EasingCurve.Linear, 4)),
      selectionGeometry = Some(
        SelectionGeometryState(
          List(
            SelectionLineGeometry(
              SelectionLineKey(0, 0),
              Tween(LayoutRect(0, 0, 0, 1), LayoutRect(0, 0, 4, 1), EasingCurve.Linear, 4)
            )
          )
        )
      )
    )
    val buffer = Buffer.fromString(bufferId, "hello world").copy(editing = EditingState.fromCursors(List(cursor)))
    val surfaces = List(
      UiSurface(
        paletteGhost,
        SurfaceContent.GhostOverlay(SurfaceContent.CommandPalette(CommandRunner.empty), LayoutRect(0, 0, 20, 4)),
        SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
      ),
      UiSurface(
        panelGhost,
        SurfaceContent.GhostOverlay(SurfaceContent.Outline(Nil), LayoutRect(0, 0, 20, 10)),
        SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
      )
    )
    base.copy(
      persisted = base.persisted.copy(buffers = base.persisted.buffers.updated(bufferId, buffer)),
      runtime = base.runtime.copy(
        uiSurfaces = base.runtime.uiSurfaces ++ surfaces,
        themeTransition = Some(ThemeTransition(base.persisted.theme, 0, 2)),
        surfaceAnimations =
          Map(paletteId -> fadeFor(SurfacePhase.Visible), paletteGhost -> fadeFor(SurfacePhase.Exiting)),
        columnTransitions = Map(
          bufferId -> ColumnTransitionState.seeded(4, EasingCurve.Linear, TransitionDirection.LeftToRight, 0, 0)
        ),
        panelGeometry = Map(panelGhost -> geometry)
      )
    )

  private val bufferAnimations: Map[BufferId, AnimationState] =
    Map(
      bufferId -> AnimationState(
        Map(
          CharacterKey(0, 0) -> AnimatedCell(Some('h'), Some(tween(4)), None, AnimationOwner.EditorText),
          CharacterKey(1, 0) -> AnimatedCell(Some('e'), Some(tween(4)), None, AnimationOwner.UiTransitions)
        )
      )
    )

  private def cursorsOf(state: AppState): List[Cursor] = state.persisted.buffers(bufferId).editing.cursors.toList

  private def ownersOf(animations: Map[BufferId, AnimationState]): Set[AnimationOwner] =
    animations(bufferId).animations.values.map(_.owner).toSet

  "the in-flight fixture" should "itself be a valid state" in {
    AppStateValidation.validated(inFlight).isRight shouldBe true
  }

  "MotionCancellation.between" should "cancel everything when motion as a whole goes from on to off" in {
    val previous = AppConfig.default
    val current  = previous.withMotionAccessibility(MotionAccessibility.Off)

    MotionCancellation.between(previous, current) shouldBe MotionCancellation.Everything
  }

  it should "cancel only the families that went from enabled to disabled" in {
    val previous = AppConfig.default
    val current  = previous.withCursorTransitionSpeedScale(Some(0.0))

    MotionCancellation.between(previous, current) shouldBe MotionCancellation.Families(List(MotionFamily.Cursor))
  }

  it should "cancel nothing when no family changed" in {
    MotionCancellation.between(AppConfig.default, AppConfig.default).isEmpty shouldBe true
  }

  "cancelling everything" should "clear every family's in-flight state and still validate" in {
    val cancelled = MotionCancellation.Everything.cancelState(inFlight)

    cancelled.runtime.themeTransition shouldBe None
    cancelled.runtime.surfaceAnimations shouldBe empty
    cancelled.runtime.columnTransitions shouldBe empty
    cancelled.runtime.panelGeometry shouldBe empty
    cancelled.runtime.uiSurfaces.map(_.id) should not contain paletteGhost
    cancelled.runtime.uiSurfaces.map(_.id) should not contain panelGhost
    cursorsOf(cancelled).map(_.glide) shouldBe List(None)
    cursorsOf(cancelled).map(_.selectionGeometry) shouldBe List(None)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  it should "clear every buffer animation regardless of owner" in {
    val cancelled = MotionCancellation.Everything.cancelBufferAnimations(bufferAnimations)

    cancelled(bufferId).hasActiveAnimations shouldBe false
  }

  "cancelling the EditorText family" should "clear only EditorText-owned buffer animations and leave state alone" in {
    val cancellation = MotionCancellation.Families(List(MotionFamily.EditorText))

    ownersOf(cancellation.cancelBufferAnimations(bufferAnimations)) shouldBe Set(AnimationOwner.UiTransitions)
    cancellation.cancelState(inFlight) shouldBe inFlight
  }

  "cancelling the CommandSurfaces family" should "drop command ghosts and command-surface fades and still validate" in {
    val cancelled = MotionCancellation.Families(List(MotionFamily.CommandSurfaces)).cancelState(inFlight)

    cancelled.runtime.uiSurfaces.map(_.id) should not contain paletteGhost
    cancelled.runtime.uiSurfaces.map(_.id) should contain(panelGhost)
    cancelled.runtime.surfaceAnimations.keySet should not contain paletteGhost
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the PinnedPanels family" should "drop a docked panel's fade and still validate" in {
    val pinned   = PanelStateReducer.pin(PanelContent.Outline(Nil), PanelPosition.Left, 24, inFlight).state
    val panelIds = pinned.pinnedSurfaces.map(_.id).filterNot(inFlight.pinnedSurfaces.map(_.id).contains)
    val seeded = pinned.copy(runtime =
      pinned.runtime.copy(surfaceAnimations =
        pinned.runtime.surfaceAnimations ++ panelIds.map(_ -> fadeFor(SurfacePhase.Visible))
      )
    )

    val cancelled = MotionCancellation.Families(List(MotionFamily.PinnedPanels)).cancelState(seeded)

    panelIds should not be empty
    cancelled.runtime.surfaceAnimations.keySet.intersect(panelIds.toSet) shouldBe empty
    cancelled.runtime.surfaceAnimations.keySet should contain(paletteGhost)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the UiTransitions family" should "clear the theme transition and UiTransitions buffer animations" in {
    val cancellation = MotionCancellation.Families(List(MotionFamily.UiTransitions))
    val cancelled    = cancellation.cancelState(inFlight)

    cancelled.runtime.themeTransition shouldBe None
    ownersOf(cancellation.cancelBufferAnimations(bufferAnimations)) shouldBe Set(AnimationOwner.EditorText)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the Cursor family" should "clear every cursor glide, leave selection geometry, and still validate" in {
    val cancelled = MotionCancellation.Families(List(MotionFamily.Cursor)).cancelState(inFlight)

    cursorsOf(cancelled).map(_.glide) shouldBe List(None)
    cursorsOf(cancelled).map(_.selectionGeometry.isDefined) shouldBe List(true)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the SelectionGeometry family" should "clear every selection geometry, leave glides, and validate" in {
    val cancelled = MotionCancellation.Families(List(MotionFamily.SelectionGeometry)).cancelState(inFlight)

    cursorsOf(cancelled).map(_.selectionGeometry) shouldBe List(None)
    cursorsOf(cancelled).map(_.glide.isDefined) shouldBe List(true)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the ColumnTransitions family" should "clear every column transition and still validate" in {
    val cancelled = MotionCancellation.Families(List(MotionFamily.ColumnTransitions)).cancelState(inFlight)

    cancelled.runtime.columnTransitions shouldBe empty
    cancelled.runtime.panelGeometry should not be empty
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }

  "cancelling the PanelGeometry family" should "clear geometry and drop a ghost that existed only for it" in {
    val cancelled = MotionCancellation.Families(List(MotionFamily.PanelGeometry)).cancelState(inFlight)

    cancelled.runtime.panelGeometry shouldBe empty
    cancelled.runtime.uiSurfaces.map(_.id) should not contain panelGhost
    cancelled.runtime.uiSurfaces.map(_.id) should contain(paletteGhost)
    AppStateValidation.validated(cancelled).isRight shouldBe true
  }
