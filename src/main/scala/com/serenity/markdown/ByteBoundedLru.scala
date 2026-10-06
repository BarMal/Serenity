package com.serenity.markdown

import java.util.LinkedHashMap

import scala.jdk.CollectionConverters.*

/** LRU cache bounded by the summed `weigh` of its values rather than by entry count, so a handful of full-panel images
  * cannot hide behind a generous entry limit. Mutated in place under `synchronized`, the same model as the other
  * [[MarkdownPreviewCache]] maps (the callers are synchronous paint-path code, not IO fibers).
  *
  * The most recently stored or read entry is never evicted, even when it alone exceeds `maxBytes`: it is the image the
  * caller is displaying right now. `supersedes(newKey, existingKey)` lets a store drop stale siblings (a resize of the
  * same slot) eagerly instead of waiting for them to age out.
  */
final private[markdown] class ByteBoundedLru[K, V](
    maxBytes: Long,
    weigh: V => Long,
    supersedes: (K, K) => Boolean = (_: K, _: K) => false
):

  private val entries = new LinkedHashMap[K, V](16, 0.75f, true)

  def get(key: K): Option[V] =
    entries.synchronized(Option(entries.get(key)))

  /** Newest entry (by recency of use) whose key satisfies `matches`; counts as a use of that entry. */
  def findNewest(matches: K => Boolean): Option[V] =
    entries.synchronized {
      entries.keySet.asScala.filter(matches).lastOption.flatMap(key => Option(entries.get(key)))
    }

  def put(key: K, value: V): Unit =
    entries.synchronized {
      entries.keySet.asScala.filter(existing => existing != key && supersedes(key, existing)).toList.foreach { stale =>
        val _ = entries.remove(stale)
      }
      val _ = entries.put(key, value)
      evictEldestBeyondBudget()
    }

  def retainedBytes: Long =
    entries.synchronized(retainedBytesLocked)

  def size: Int =
    entries.synchronized(entries.size)

  private def retainedBytesLocked: Long =
    entries.values.asScala.map(weigh).sum

  private def evictEldestBeyondBudget(): Unit =
    val eldestFirst = entries.keySet.asScala.toList
    val newest      = eldestFirst.lastOption
    val _ = eldestFirst
      .filterNot(newest.contains)
      .foldLeft(retainedBytesLocked) { (retained, key) =>
        if retained <= maxBytes then retained
        else retained - Option(entries.remove(key)).map(weigh).getOrElse(0L)
      }

end ByteBoundedLru
