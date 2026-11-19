package com.serenity.ui.skia

import java.lang.ref.{ReferenceQueue, WeakReference}
import java.util.concurrent.ConcurrentHashMap

/** A memo that lets go of a value once nothing else holds it: whoever made the value for a key keeps it alive, and
  * anyone who asks for the same key meanwhile is given that very value. The memo itself adds nothing to what is
  * resident, and an entry goes with its value.
  */
final private[skia] class WeakMemo[K <: AnyRef, V <: AnyRef]:

  final private class Entry(val key: K, value: V, queue: ReferenceQueue[V]) extends WeakReference[V](value, queue)

  private val entries = new ConcurrentHashMap[K, Entry]()
  private val dropped = new ReferenceQueue[V]()

  def getOrCompute(key: K)(make: => V): V =
    forgetDropped()
    Option(entries.get(key)).flatMap(entry => Option(entry.get)).getOrElse {
      val value = make
      val _     = entries.put(key, new Entry(key, value, dropped))
      value
    }

  def size: Int = entries.size

  @annotation.tailrec
  private def forgetDropped(): Unit =
    Option(dropped.poll()).collect { case entry: Entry => entry } match
      case Some(entry) =>
        val _ = entries.remove(entry.key, entry)
        forgetDropped()
      case None => ()
