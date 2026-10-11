package com.serenity.state.models

/** Selecting the best few of many without ordering all of them. */
object TopK:

  /** The `k` smallest of `items` under `order`, ascending, equal items in their input order -- the same as
    * `items.toVector.sorted.take(k)`. An item is compared once against the current `k`-th smallest and only those that
    * beat it are kept, to be sorted in with the `k` so far each time `k` of them have gathered.
    */
  def smallest[A](items: Iterable[A], k: Int)(using order: Ordering[A]): Vector[A] =
    if k <= 0 then Vector.empty
    else items.foldLeft(Selection.empty[A])(_.offered(_, k)).settled(k)

  final private case class Selection[A](kept: Vector[A], candidates: Vector[A])(using order: Ordering[A]):

    def offered(item: A, k: Int): Selection[A] =
      val beatsKth = kept.sizeIs < k || kept.lastOption.exists(kth => order.lt(item, kth))
      if !beatsKth then this
      else
        val gathered = copy(candidates = candidates :+ item)
        if gathered.candidates.sizeIs >= k then Selection(gathered.settled(k), Vector.empty) else gathered

    def settled(k: Int): Vector[A] = (kept ++ candidates).sorted.take(k)

  private object Selection:
    def empty[A](using Ordering[A]): Selection[A] = Selection(Vector.empty, Vector.empty)
