package com.serenity.state.manager

import cats.effect.IO
import com.serenity.animation.{AnimationOwner, AnimationState}
import com.serenity.config.{AppConfig, MotionFamily}
import com.serenity.state.models.*

/** Cancels in-flight animation state for one or every motion family, called whenever a config change turns a
  * previously-enabled family off (`StateManagerConfigEffects.updateMotionConfig`'s `cancelDisabledMotion` call). The
  * decision and its effect on state are the pure [[MotionCancellation]]; this shell commits it to the app state and the
  * buffer animations in one model write.
  */
final private[manager] class StateManagerMotionCancellation(
    updateModelValidated: (Model => Option[Model]) => IO[Unit]
):

  def cancelActiveMotion(): IO[Unit] = cancel(MotionCancellation.Everything)

  def cancelDisabledMotion(previous: AppConfig, current: AppConfig): IO[Unit] =
    cancel(MotionCancellation.between(previous, current))

  private def cancel(cancellation: MotionCancellation): IO[Unit] =
    if cancellation.isEmpty then IO.unit
    else
      updateModelValidated(model =>
        Some(
          model.copy(
            app = cancellation.cancelState(model.app),
            bufferAnimations = cancellation.cancelBufferAnimations(model.bufferAnimations)
          )
        )
      )

/** Which in-flight motion a config change cancels: everything once motion as a whole goes off, otherwise just the
  * families that went from enabled to disabled.
  */
private[manager] enum MotionCancellation:
  case Everything
  case Families(families: List[MotionFamily])

  def isEmpty: Boolean =
    this match
      case Everything         => false
      case Families(families) => families.isEmpty

  def cancelState(state: AppState): AppState =
    this match
      case Everything => MotionCancellation.cancelAllState(state)
      case Families(families) =>
        families.foldLeft(state)((current, family) => MotionCancellation.cancelFamilyState(family, current))

  def cancelBufferAnimations(animations: Map[BufferId, AnimationState]): Map[BufferId, AnimationState] =
    this match
      case Everything => animations.view.mapValues(_.clearAll()).toMap
      case Families(families) =>
        families
          .flatMap(MotionCancellation.bufferAnimationOwner)
          .foldLeft(animations)((current, owner) => current.view.mapValues(_.clear(owner)).toMap)

private[manager] object MotionCancellation:

  def between(previous: AppConfig, current: AppConfig): MotionCancellation =
    val previousFamilies = previous.surfaceConfig.effectiveMotionConfiguration
    val currentFamilies  = current.surfaceConfig.effectiveMotionConfiguration
    if currentFamilies.families.values.forall(!_.enabled) && previousFamilies.families.values.exists(_.enabled) then
      Everything
    else
      Families(
        MotionFamily.values.toList
          .filter(family => previousFamilies.family(family).enabled && !currentFamilies.family(family).enabled)
      )

  private def bufferAnimationOwner(family: MotionFamily): Option[AnimationOwner] =
    family match
      case MotionFamily.EditorText    => Some(AnimationOwner.EditorText)
      case MotionFamily.UiTransitions => Some(AnimationOwner.UiTransitions)
      case _                          => None

  private def cancelAllState(state: AppState): AppState =
    state.copy(
      persisted = state.persisted.copy(buffers =
        state.persisted.buffers.view.mapValues(clearCursorGlides andThen clearSelectionGeometries).toMap
      ),
      runtime = state.runtime.copy(
        themeTransition = None,
        uiSurfaces = state.runtime.uiSurfaces.filterNot(isGhostOverlay),
        surfaceAnimations = Map.empty,
        companionSprite = state.runtime.companionSprite.resetTyping,
        columnTransitions = Map.empty,
        panelGeometry = Map.empty
      )
    )

  private def cancelFamilyState(family: MotionFamily, state: AppState): AppState =
    family match
      case MotionFamily.EditorText      => state
      case MotionFamily.CommandSurfaces => cancelSurfaceMotion(isCommandSurface, state)
      case MotionFamily.PinnedPanels    => cancelSurfaceMotion(isDockedSurface, state)
      case MotionFamily.UiTransitions =>
        state.copy(runtime =
          state.runtime.copy(themeTransition = None, companionSprite = state.runtime.companionSprite.resetTyping)
        )
      case MotionFamily.Cursor =>
        // Caret-glide (issue #1085 phase 2): clears every buffer's in-flight `Cursor.glide` -- the one piece of
        // `Cursor`-family state that is actually cancellable (unlike blink/breathe cadence, which has no state to
        // cancel, only a tick interval it stops requesting).
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers.view.mapValues(clearCursorGlides).toMap)
        )
      case MotionFamily.SelectionGeometry =>
        // Selection grow/settle (issue #1085 phase 3): clears every buffer's in-flight `Cursor.selectionGeometry` --
        // the one piece of `SelectionGeometry`-family state that is actually cancellable, the same way `Cursor`'s own
        // case clears `Cursor.glide`.
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers.view.mapValues(clearSelectionGeometries).toMap)
        )
      case MotionFamily.ColumnTransitions =>
        state.copy(runtime = state.runtime.copy(columnTransitions = Map.empty))
      case MotionFamily.PanelGeometry =>
        // Clears every in-flight scale-in/out, then drops any close ghost that existed only for this geometry (no
        // `surfaceAnimations` entry of its own) -- otherwise, with its geometry gone and no colour fade left to
        // eventually remove it (`AnimationChoreography.advancePanelGeometry`'s ordinary path), it would sit in
        // `uiSurfaces` forever.
        val orphanedGhostIds = state.runtime.panelGeometry.keySet.filterNot(state.runtime.surfaceAnimations.contains)
        val ghostIdsToDrop = state.runtime.uiSurfaces.collect {
          case UiSurface(id, SurfaceContent.GhostOverlay(_, _), _, _) if orphanedGhostIds.contains(id) => id
        }.toSet
        state.copy(runtime =
          state.runtime.copy(
            panelGeometry = Map.empty,
            uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => ghostIdsToDrop.contains(surface.id))
          )
        )

  private def clearCursorGlides(buffer: Buffer): Buffer =
    if buffer.editing.cursors.exists(_.glide.isDefined) then
      buffer.withCursorList(buffer.editing.cursors.map(_.copy(glide = None)))
    else buffer

  private def clearSelectionGeometries(buffer: Buffer): Buffer =
    if buffer.editing.cursors.exists(_.selectionGeometry.isDefined) then
      buffer.withCursorList(buffer.editing.cursors.map(_.copy(selectionGeometry = None)))
    else buffer

  private def cancelSurfaceMotion(matches: UiSurface => Boolean, state: AppState): AppState =
    val matchingIds = state.runtime.uiSurfaces.collect { case surface if matches(surface) => surface.id }.toSet
    state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => matches(surface) && isGhostOverlay(surface)),
        surfaceAnimations = state.runtime.surfaceAnimations.filterNot((surfaceId, _) => matchingIds.contains(surfaceId))
      )
    )

  private def isCommandSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommandPalette(_) => true
      case SurfaceContent.GhostOverlay(content, _) =>
        content match
          case SurfaceContent.CommandPalette(_) => true
          case _                                => false
      case _ => false

  /** A transient close-fade ghost, which is discarded rather than animated when motion is cancelled. */
  private def isGhostOverlay(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.GhostOverlay(_, _) => true
      case _                                 => false

  /** A surface occupying a workspace dock, whether at its pinned size or expanded over the editor. */
  private def isDockedSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Docked => true
      case _                          => false
