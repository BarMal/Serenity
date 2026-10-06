package com.serenity.rope

import scala.annotation.tailrec

/** Composition of two [[ChangeSet]]s by walking both as a run of kept and edited stretches: the first change's output
  * is the second change's input, so the walk consumes them in lockstep, one overlap at a time.
  */
private[rope] object ChangeSetCompose:

  private enum Op:
    case Keep(length: Int)
    case Edit(removed: Int, insert: String)

  /** `second` must start from the document `first` produces. */
  def compose(first: ChangeSet, second: ChangeSet): ChangeSet =
    val ops = walk(toOps(first), toOps(second), Nil).reverse
    ChangeSet.canonical(first.oldLength, replacements(ops))

  private def toOps(changes: ChangeSet): List[Op] =
    val (ops, end) = changes.parts.foldLeft((List.empty[Op], 0)) {
      case ((acc, at), part) =>
        val kept = if part.from > at then Op.Keep(part.from - at) :: acc else acc
        (Op.Edit(part.removedLength, part.insert) :: kept, part.to)
    }
    (if end < changes.oldLength then Op.Keep(changes.oldLength - end) :: ops else ops).reverse

  private def replacements(ops: List[Op]): List[Replacement] =
    ops
      .foldLeft((List.empty[Replacement], 0)) {
        case ((acc, at), Op.Keep(length)) => (acc, at + length)
        case ((acc, at), Op.Edit(removed, insert)) =>
          (Replacement(at, at + removed, insert) :: acc, at + removed)
      }
      ._1
      .reverse

  /** A deletion by `first` produces nothing for `second` to see, and an insertion by `second` consumes nothing of what
    * `first` produced, so both pass straight through. Every other pair is matched over the characters they share.
    */
  @tailrec
  private def walk(first: List[Op], second: List[Op], out: List[Op]): List[Op] =
    (first, second) match
      case (Op.Edit(removed, "") :: firstRest, _) =>
        walk(firstRest, second, Op.Edit(removed, "") :: out)
      case (_, Op.Edit(0, insert) :: secondRest) =>
        walk(first, secondRest, Op.Edit(0, insert) :: out)
      case (head :: firstRest, other :: secondRest) =>
        val shared                           = math.min(produced(head), consumed(other))
        val (emitted, firstLeft, secondLeft) = overlap(head, other, shared)
        walk(firstLeft.toList ++ firstRest, secondLeft.toList ++ secondRest, emitted :: out)
      case _ => out

  private def produced(op: Op): Int = op match
    case Op.Keep(length)    => length
    case Op.Edit(_, insert) => insert.length

  private def consumed(op: Op): Int = op match
    case Op.Keep(length)     => length
    case Op.Edit(removed, _) => removed

  /** Matches `shared` characters of what `first` produces against what `second` consumes, returning the combined edit
    * and what is left of each head. The text `second` inserts is emitted with its first matched stretch only.
    */
  private def overlap(first: Op, second: Op, shared: Int): (Op, Option[Op], Option[Op]) =
    val secondLeft = second match
      case Op.Keep(length)     => Option.when(length > shared)(Op.Keep(length - shared))
      case Op.Edit(removed, _) => Option.when(removed > shared)(Op.Edit(removed - shared, ""))
    first match
      case Op.Keep(length) =>
        val emitted = second match
          case Op.Keep(_)         => Op.Keep(shared)
          case Op.Edit(_, insert) => Op.Edit(shared, insert)
        (emitted, Option.when(length > shared)(Op.Keep(length - shared)), secondLeft)
      case Op.Edit(removed, insert) =>
        val emitted = second match
          case Op.Keep(_)             => Op.Edit(removed, insert.take(shared))
          case Op.Edit(_, secondText) => Op.Edit(removed, secondText)
        (emitted, Option.when(insert.length > shared)(Op.Edit(0, insert.drop(shared))), secondLeft)
