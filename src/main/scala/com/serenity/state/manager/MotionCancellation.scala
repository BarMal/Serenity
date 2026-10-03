package com.serenity.state.manager

import com.serenity.animation.{AnimationOwner, AnimationState}
import com.serenity.config.{AppConfig, MotionFamily}
import com.serenity.state.models.*

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
      runtime = state.runtime.copy(
        themeDiscovery = state.runtime.themeDiscovery.copy(transition = None),
        uiSurfaces = state.runtime.uiSurfaces.filterNot(isGhostOverlay),
        motion = MotionState()
      )
    )

  private def cancelFamilyState(family: MotionFamily, state: AppState): AppState =
    family match
      case MotionFamily.EditorText      => state
      case MotionFamily.CommandSurfaces => cancelSurfaceMotion(isCommandSurface, state)
      case MotionFamily.PinnedPanels    => cancelSurfaceMotion(isDockedSurface, state)
      case MotionFamily.UiTransitions =>
        state.copy(runtime = state.runtime.copy(themeDiscovery = state.runtime.themeDiscovery.copy(transition = None)))
      case MotionFamily.Cursor | MotionFamily.SelectionGeometry | MotionFamily.ColumnTransitions |
          MotionFamily.PanelGeometry =>
        state

  private def cancelSurfaceMotion(matches: UiSurface => Boolean, state: AppState): AppState =
    val matchingIds = state.runtime.uiSurfaces.collect { case surface if matches(surface) => surface.id }.toSet
    state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => matches(surface) && isGhostOverlay(surface)),
        motion = state.runtime.motion.copy(surfaceAnimations =
          state.runtime.motion.surfaceAnimations.filterNot((surfaceId, _) => matchingIds.contains(surfaceId))
        )
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
