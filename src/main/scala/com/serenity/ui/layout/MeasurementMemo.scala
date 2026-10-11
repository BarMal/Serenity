package com.serenity.ui.layout

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

/** A measurement that is a pure function of its key, computed once and served from memory afterwards. Keys are values
  * (`Font` and `FontRenderContext` compare structurally), so a changed font or scale misses and nothing here goes
  * stale; `maxEntries` only stops a long session of ever-different keys from growing the map without limit.
  */
final private[layout] class MeasurementMemo[K, V](maxEntries: Int)(measure: K => V):

  private val entries  = new ConcurrentHashMap[K, V]()
  private val computed = new AtomicLong(0)

  /** How many keys were measured, as opposed to served from the memo. */
  def measurements: Long = computed.get

  def apply(key: K): V =
    if entries.size >= maxEntries && !entries.containsKey(key) then entries.clear()
    entries.computeIfAbsent(
      key,
      _ =>
        val _ = computed.incrementAndGet()
        measure(key)
    )
