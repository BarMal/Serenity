package com.serenity.facts

import java.time.{Duration, Instant}

/** A project root's folder in the cache, named by the hash of its real path. */
opaque type RootKey = String

object RootKey:
  def apply(value: String): RootKey = value

  extension (key: RootKey) def value: String = key

final case class RootUsage(root: RootKey, sizeBytes: Long, lastOpened: Instant, open: Boolean)

/** How big the cache may grow (#2531 §2.4). Every value comes from F0's measurements and the owner's sign-off, so none
  * has a default here.
  */
final case class EvictionBounds(globalCapBytes: Long, maxAge: Duration, orphanShare: Double)

/** A stored fact no current file refers to: kept for a while so switching back to a branch reuses it. */
final case class OrphanCandidate(sizeBytes: Long, lastSeenSession: Long)

/** What to remove from the fact store, decided from sizes and dates alone (#2531 §2.4). */
object FactEviction:

  /** Roots not opened within the age limit, then the least recently opened until the rest fit under the cap, oldest
    * first. An open root is never removed.
    */
  def rootsToRemove(roots: List[RootUsage], bounds: EvictionBounds, now: Instant): List[RootKey] =
    val removable          = roots.filterNot(_.open).sortBy(_.lastOpened)
    val (aged, recent)     = removable.partition(_.lastOpened.isBefore(now.minus(bounds.maxAge)))
    val remainingAfterAged = roots.map(_.sizeBytes).sum - aged.map(_.sizeBytes).sum
    val overCap = recent
      .scanLeft(remainingAfterAged)(_ - _.sizeBytes)
      .zip(recent)
      .takeWhile((remaining, _) => remaining > bounds.globalCapBytes)
      .map(_._2)
    (aged ++ overCap).map(_.root)

  /** The orphans beyond their share of the root's live size, keeping the most recently seen. */
  def orphansToDrop(
    liveBytes: Long,
    orphans: Map[ContentHash, OrphanCandidate],
    bounds: EvictionBounds
  ): Set[ContentHash] =
    val budget    = (liveBytes * bounds.orphanShare).toLong
    val byRecency = orphans.toVector.sortBy((_, orphan) => -orphan.lastSeenSession)
    val kept = byRecency
      .scanLeft(0L)(_ + _._2.sizeBytes)
      .drop(1)
      .zip(byRecency)
      .takeWhile((used, _) => used <= budget)
      .map(_._2._1)
      .toSet
    orphans.keySet -- kept

  def staleVersions(stored: Set[ProducerVersion], current: ProducerVersion): Set[ProducerVersion] = stored - current
