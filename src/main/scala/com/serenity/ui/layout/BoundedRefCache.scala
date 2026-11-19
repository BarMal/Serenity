package com.serenity.ui.layout

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.collection.immutable.VectorMap

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

  /** Oldest write first: a write moves its key to the end, and the oldest are dropped from the front. */
  private val state: AtomicReference[VectorMap[K, V]] = new AtomicReference(VectorMap.empty)

  private def bounded(entries: VectorMap[K, V]): VectorMap[K, V] =
    val excess = entries.size - capacity.get()
    if excess <= 0 then entries else entries -- entries.keysIterator.take(excess).toList

  /** Retries `f` against the current entries until its compare-and-set succeeds -- the plain-value equivalent of
    * `Ref.update`.
    */
  @annotation.tailrec
  private def casUpdate(f: VectorMap[K, V] => VectorMap[K, V]): Unit =
    val current = state.get()
    val next    = f(current)
    if !state.compareAndSet(current, next) then casUpdate(f)

  /** As [[casUpdate]], but `f` also reports a value computed alongside the transform -- the plain-value equivalent of
    * `Ref.modify`.
    */
  @annotation.tailrec
  private def casModify[A](f: VectorMap[K, V] => (VectorMap[K, V], A)): A =
    val current        = state.get()
    val (next, result) = f(current)
    if state.compareAndSet(current, next) then result else casModify(f)

  def get(key: K): Option[V] =
    state.get().get(key)

  def snapshot: Map[K, V] =
    state.get()

  def put(key: K, value: V): Unit =
    casUpdate(entries => bounded((entries - key).updated(key, value)))

  def remove(key: K): Unit =
    casUpdate(_ - key)

  /** Rewrites every entry at once (e.g. folding damage into several tracked identities together) rather than one key at
    * a time.
    */
  def replaceAll(f: Map[K, V] => Map[K, V]): Unit =
    modify(entries => (f(entries), ()))

  /** As [[casModify]], scoped to just the entries map: transforms the whole map and reports a value computed alongside
    * that transform, atomically. The entries that stay keep their places and new ones come last.
    */
  def modify[A](f: Map[K, V] => (Map[K, V], A)): A =
    casModify { current =>
      val (next, result) = f(current)
      val kept           = current.iterator.collect { case (key, _) if next.contains(key) => key -> next(key) }
      val added          = next.iterator.filterNot((key, _) => current.contains(key))
      (bounded(VectorMap.from(kept ++ added)), result)
    }
