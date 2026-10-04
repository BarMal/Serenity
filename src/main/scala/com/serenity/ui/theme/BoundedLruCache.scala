package com.serenity.ui.theme

import java.util.LinkedHashMap

/** A size-bounded, least-recently-used memo with hashed O(1) lookup, safe to share between threads. The same
  * synchronized access-ordered `LinkedHashMap` shape as `GraphemeSegmentationCache`: contained private mutation that
  * never leaks past this class (docs/coding-standards.md, Mutation Policy). Every access, a hit included, is a
  * structural modification of an access-ordered map, so reads take the lock too.
  */
final private[theme] class BoundedLruCache[K, V](maxEntries: Int):

  private val entries =
    new LinkedHashMap[K, V](16, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean =
        size() > maxEntries

  /** `compute` runs outside the lock, so two threads missing on the same key may both compute it; the last write wins,
    * which is harmless for a pure `compute`.
    */
  def getOrCompute(key: K)(compute: => V): V =
    entries.synchronized(Option(entries.get(key))) match
      case Some(cached) => cached
      case None =>
        val computed = compute
        entries.synchronized {
          val _ = entries.put(key, computed)
        }
        computed

  def entryCount: Int = entries.synchronized(entries.size())
