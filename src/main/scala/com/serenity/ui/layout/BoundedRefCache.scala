package com.serenity.ui.layout

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

/** An `AtomicReference[Map[K, V]]`-backed cache bounded to
  * [[com.serenity.ui.renderer.RendererFrameState.cacheCapacity]] most-recently- written entries, standing in for
  * `java.util.WeakHashMap`'s GC-driven eviction (see that constant's doc comment for why a recency bound is an
  * equivalent, and simpler, way to keep these process-wide caches from growing without limit). A manual compare-and-set
  * retry loop ([[casUpdate]]/[[casModify]]) replaces the manual `synchronized` blocks this module used to need for the
  * same "render thread isn't guaranteed to be one thread" reason -- this module was briefly `Ref[IO, ...]`-backed
  * (#1431), but every accessor forced that `IO` synchronously via `unsafeRunSync` right back out again to keep the
  * module's long-standing synchronous API, which just hid a plain in-memory CAS behind an effect type nothing here ever
  * suspended on (#1434). `AtomicReference` gives the same lock-free CAS semantics without the indirection.
  */
final private[ui] class BoundedRefCache[K, V](capacity: AtomicInteger):
  private case class Contents(entries: Map[K, V], order: Vector[K])

  private val state: AtomicReference[Contents] = new AtomicReference(Contents(Map.empty, Vector.empty))

  private def bounded(contents: Contents): Contents =
    val currentCapacity = capacity.get()
    if contents.order.size <= currentCapacity then contents
    else
      val dropCount = contents.order.size - currentCapacity
      val dropped   = contents.order.take(dropCount)
      Contents(contents.entries -- dropped, contents.order.drop(dropCount))

  /** Retries `f` against the current [[Contents]] until its compare-and-set succeeds -- the plain-value equivalent of
    * `Ref.update`.
    */
  @annotation.tailrec
  private def casUpdate(f: Contents => Contents): Unit =
    val current = state.get()
    val next    = f(current)
    if !state.compareAndSet(current, next) then casUpdate(f)

  /** As [[casUpdate]], but `f` also reports a value computed alongside the transform -- the plain-value equivalent of
    * `Ref.modify`.
    */
  @annotation.tailrec
  private def casModify[A](f: Contents => (Contents, A)): A =
    val current        = state.get()
    val (next, result) = f(current)
    if state.compareAndSet(current, next) then result else casModify(f)

  def get(key: K): Option[V] =
    state.get().entries.get(key)

  def snapshot: Map[K, V] =
    state.get().entries

  def put(key: K, value: V): Unit =
    casUpdate(c => bounded(Contents(c.entries.updated(key, value), c.order.filterNot(_ == key) :+ key)))

  def remove(key: K): Unit =
    casUpdate(c => Contents(c.entries - key, c.order.filterNot(_ == key)))

  /** Rewrites every entry at once (e.g. folding damage into several tracked identities together) rather than one key at
    * a time.
    */
  def replaceAll(f: Map[K, V] => Map[K, V]): Unit =
    modify(entries => (f(entries), ()))

  /** As [[casModify]], scoped to just the entries map: transforms the whole map and reports a value computed alongside
    * that transform, atomically.
    */
  def modify[A](f: Map[K, V] => (Map[K, V], A)): A =
    casModify { c =>
      val (nextEntries, result) = f(c.entries)
      val nextOrder = c.order.filter(nextEntries.contains) ++ nextEntries.keySet.diff(c.order.toSet).toVector
      (bounded(Contents(nextEntries, nextOrder)), result)
    }
