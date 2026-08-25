package com.serenity.state.manager

import cats.syntax.all.*
import com.serenity.rope.{Balance, RopeDiff}
import com.serenity.state.models.*

/** Computes what a transition between two `AppState`s damaged, so the render loop doesn't have to rediscover it by
  * diffing frames (`Renderer.planFrame`'s `ChromeKey`/`dirtyRowsAgainst` machinery, `#999`). Produced once, centrally,
  * at the same effect-boundary funnel points as `CursorViewport.ensureVisibleCursors` -- see that object's doc comment
  * for why per-reducer-branch emission was rejected in favour of a boundary pass.
  *
  * Buffer content damage goes through `RopeDiff`, which finds the changed offset range by walking the rope's persistent
  * tree structure rather than comparing text, so its cost tracks how much of the document an edit actually touched
  * rather than the document's size.
  *
  * Landed unused: nothing calls this yet. `#998` wires it in alongside the damage queue that replaces `fastMode`.
  */
object DamageProducer:

  def forTransition(before: AppState, after: AppState)(using Balance): Damage =
    val bufferDamage = after.buffers.foldLeft(Damage.Nothing: Damage) {
      case (acc, (bufferId, afterBuffer)) =>
        before.buffers.get(bufferId) match
          case None               => acc
          case Some(beforeBuffer) => acc |+| contentDamage(bufferId, beforeBuffer, afterBuffer)
    }
    bufferDamage |+| chromeDamage(before, after)

  private def contentDamage(bufferId: BufferId, before: Buffer, after: Buffer)(using Balance): Damage =
    if isSameReference(before.content, after.content) then Damage.Nothing
    else
      RopeDiff.changedOffsetRange(before.content, after.content) match
        case None               => Damage.Nothing
        case Some((start, end)) => Damage.BufferRows(bufferId, rowsForOffsetRange(after, start, end))

  /** The lines `[start, end)` (an exclusive offset range in `after`'s content) touches. A pure deletion reports
    * `end == start`, which still damages the one line the deletion landed on, so the last-affected offset is clamped to
    * be at least `start` rather than `end - 1` going negative relative to it.
    */
  private def rowsForOffsetRange(after: Buffer, start: Int, end: Int): Set[Int] =
    val weight         = after.content.weight
    val clampedStart   = math.max(0, math.min(start, weight))
    val lastOffset     = math.max(clampedStart, math.min(end, weight) - 1)
    val (startLine, _) = after.content.offsetToLineColumn(clampedStart)
    val (endLine, _)   = after.content.offsetToLineColumn(lastOffset)
    (startLine to endLine).toSet

  private def chromeDamage(before: AppState, after: AppState): Damage =
    if before.theme != after.theme then Damage.Chrome else Damage.Nothing

  private def isSameReference(a: AnyRef, b: AnyRef): Boolean = a eq b
