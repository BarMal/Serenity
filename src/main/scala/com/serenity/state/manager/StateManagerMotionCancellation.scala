package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.state.models.*

/** Cancels in-flight animation state for one or every motion family, called whenever a config change turns a
  * previously-enabled family off (`StateManagerConfigEffects.updateMotionConfig`'s `cancelDisabledMotion` call). Split
  * out of `StateManagerConfigEffects` (which otherwise crossed this session's architecture ratchet) as its own
  * self-contained cluster -- everything here is either called from [[cancelDisabledMotion]] or from another method in
  * this same file.
  */
final private[manager] class StateManagerMotionCancellation(
    stateRef: Ref[IO, AppState],
    bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]
):

  def cancelActiveMotion(): IO[Unit] =
    clearBufferAnimations() >>
      stateRef.update(state =>
        state.copy(
          persisted = state.persisted.copy(buffers = state.persisted.buffers.view.mapValues(clearCursorGlides).toMap),
          runtime = state.runtime.copy(
            themeTransition = None,
            uiSurfaces = state.runtime.uiSurfaces.filterNot(isGhostOverlay),
            surfaceAnimations = Map.empty,
            companionSprite = state.runtime.companionSprite.resetTyping,
            columnTransitions = Map.empty,
            panelGeometry = Map.empty
          )
        )
      )

  def cancelDisabledMotion(previous: AppConfig, current: AppConfig): IO[Unit] =
    val previousFamilies = previous.surfaceConfig.effectiveMotionConfiguration
    val currentFamilies  = current.surfaceConfig.effectiveMotionConfiguration
    if currentFamilies.families.values.forall(!_.enabled) && previousFamilies.families.values.exists(_.enabled) then
      cancelActiveMotion()
    else
      com.serenity.config.MotionFamily.values.toList
        .filter(family => previousFamilies.family(family).enabled && !currentFamilies.family(family).enabled)
        .traverse_(cancelMotionFamily)

  private def cancelMotionFamily(family: com.serenity.config.MotionFamily): IO[Unit] =
    family match
      case com.serenity.config.MotionFamily.EditorText =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.EditorText)
      case com.serenity.config.MotionFamily.CommandSurfaces =>
        cancelSurfaceMotion(isCommandSurface)
      case com.serenity.config.MotionFamily.PinnedPanels =>
        cancelSurfaceMotion(isDockedSurface)
      case com.serenity.config.MotionFamily.UiTransitions =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.UiTransitions) >>
          stateRef.update(state =>
            state.copy(runtime =
              state.runtime.copy(
                themeTransition = None,
                companionSprite = state.runtime.companionSprite.resetTyping
              )
            )
          )
      case com.serenity.config.MotionFamily.Cursor =>
        // Caret-glide (issue #1085 phase 2): clears every buffer's in-flight `Cursor.glide` -- the one piece of
        // `Cursor`-family state that is actually cancellable (unlike blink/breathe cadence, which has no state to
        // cancel, only a tick interval it stops requesting).
        stateRef.update(state =>
          state.copy(persisted =
            state.persisted.copy(buffers = state.persisted.buffers.view.mapValues(clearCursorGlides).toMap)
          )
        )
      case com.serenity.config.MotionFamily.ColumnTransitions =>
        stateRef.update(state => state.copy(runtime = state.runtime.copy(columnTransitions = Map.empty)))
      case com.serenity.config.MotionFamily.PanelGeometry =>
        // Clears every in-flight scale-in/out, then drops any close ghost that existed only for this geometry (no
        // `surfaceAnimations` entry of its own) -- otherwise, with its geometry gone and no colour fade left to
        // eventually remove it (`AnimationChoreography.advancePanelGeometry`'s ordinary path), it would sit in
        // `uiSurfaces` forever.
        stateRef.update { state =>
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
        }

  /** Drops every cursor's in-flight glide on `buffer`, leaving everything else untouched -- the `Cursor` family's
    * cancellation (`cancelActiveMotion`/`cancelMotionFamily`'s `Cursor` case).
    */
  private def clearCursorGlides(buffer: Buffer): Buffer =
    if buffer.editing.cursors.exists(_.glide.isDefined) then
      buffer.withCursorList(buffer.editing.cursors.map(_.copy(glide = None)))
    else buffer

  private def clearBufferAnimations(): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clearAll()
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def clearBufferAnimations(owner: com.serenity.animation.AnimationOwner): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clear(owner)
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def cancelSurfaceMotion(matches: UiSurface => Boolean): IO[Unit] =
    stateRef.update { state =>
      val matchingIds = state.runtime.uiSurfaces.collect { case surface if matches(surface) => surface.id }.toSet
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => matches(surface) && isGhostOverlay(surface)),
          surfaceAnimations =
            state.runtime.surfaceAnimations.filterNot((surfaceId, _) => matchingIds.contains(surfaceId))
        )
      )
    }

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
