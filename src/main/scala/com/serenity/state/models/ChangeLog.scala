package com.serenity.state.models

import com.serenity.rope.ChangeSet

/** The edits that took a [[Document]] from one `contentVersion` to the next, newest last, for as far back as the log
  * still holds them. It is a cache of derivable facts, never state: equality ignores it, so two documents with the same
  * text and version are equal however they got there, and it is not persisted.
  *
  * `head` is the version the entries lead to. A log whose `head` is not its document's `contentVersion` describes some
  * other history, and [[Document.changesSince]] refuses to use it.
  */
final class ChangeLog private (
    val head: Long,
    val entries: Vector[ChangeLog.Entry],
    val insertedChars: Int
):

  /** One edit made at `fromVersion`, which took the document to `fromVersion + 1`. A log appends only to its `head`, so
    * its entries are always consecutive.
    */
  def appended(fromVersion: Long, change: ChangeSet): ChangeLog =
    val base = if fromVersion == head then this else ChangeLog.at(fromVersion)
    ChangeLog.bounded(
      fromVersion + 1,
      base.entries :+ ChangeLog.Entry(fromVersion, change),
      base.insertedChars + ChangeLog.insertedBy(change)
    )

  /** An edit from `fromVersion` that was not recorded: nothing before it can be composed across any more. */
  def gapped(fromVersion: Long): ChangeLog = ChangeLog.at(fromVersion + 1)

  /** The one change from `version` to [[head]], or `None` when an entry between them was evicted or never recorded, or
    * there is nothing to compose because `version` is already the head.
    */
  def since(version: Long): Option[ChangeSet] =
    entries.dropWhile(_.fromVersion < version) match
      case first +: rest if first.fromVersion == version =>
        rest.foldLeft(Option(first.change))((composed, entry) => composed.flatMap(_.compose(entry.change)))
      case _ => None

  override def equals(other: Any): Boolean =
    other match
      case _: ChangeLog => true
      case _            => false

  override def hashCode: Int = 0

  override def toString: String =
    s"ChangeLog(head=$head, entries=${entries.size}, insertedChars=$insertedChars)"

object ChangeLog:

  final case class Entry(fromVersion: Long, change: ChangeSet)

  /** At least `TypedRuns.MaxKeys + 32`, so a run of typed keys and the edits around it stay composable. `models` cannot
    * see `TypedRuns`, so `ChangeLogSpec` holds the two together.
    */
  val Capacity = 160

  /** What a log holds of inserted text; a large paste or replace-all evicts the log instead of being retained. */
  val MaxInsertedChars = 1 << 20

  def at(version: Long): ChangeLog = new ChangeLog(version, Vector.empty, 0)

  val empty: ChangeLog = at(0L)

  private def insertedBy(change: ChangeSet): Int = change.parts.foldLeft(0)(_ + _.insert.length)

  private def bounded(head: Long, entries: Vector[Entry], insertedChars: Int): ChangeLog =
    val overflow = entries.size - Capacity
    val trimmed  = if overflow > 0 then entries.drop(overflow) else entries
    val chars =
      if overflow > 0 then trimmed.foldLeft(0)((total, entry) => total + insertedBy(entry.change)) else insertedChars
    evictedWhileOverCharCap(head, trimmed, chars)

  @scala.annotation.tailrec
  private def evictedWhileOverCharCap(head: Long, entries: Vector[Entry], insertedChars: Int): ChangeLog =
    entries match
      case oldest +: rest if insertedChars > MaxInsertedChars =>
        evictedWhileOverCharCap(head, rest, insertedChars - insertedBy(oldest.change))
      case _ => new ChangeLog(head, entries, insertedChars)
