package com.serenity.state.manager

import com.serenity.animation.{AnimationOwner, AnimationState}
import com.serenity.config.{AppConfig, MotionFamily}
import com.serenity.state.models.BufferId

/** Which in-flight buffer motion a config change cancels: everything once motion as a whole goes off, otherwise just
  * the families that went from enabled to disabled.
  */
private[manager] enum MotionCancellation:
  case Everything
  case Families(families: List[MotionFamily])

  def isEmpty: Boolean =
    this match
      case Everything         => false
      case Families(families) => families.isEmpty

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
