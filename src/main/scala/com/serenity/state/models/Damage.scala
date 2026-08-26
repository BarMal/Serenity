package com.serenity.state.models

import cats.Monoid

/** What a frame needs to repaint, reported by the code that made the change instead of rediscovered by diffing two
  * frames against each other.
  *
  * `Combined` is the only case that isn't a single fact about one target; it holds the facts that [[Monoid combine]]
  * could not fold into one another (different buffers, different panes, ...). `combine` always re-groups every leaf
  * fact by its target and reduces each group to its minimal form, so the result depends only on the multiset of facts
  * being combined -- never on which pairs were combined first or in what order -- which is what makes the instance
  * associative rather than merely "usually associative in the cases we tried".
  */
enum Damage:
  case Nothing
  case BufferCells(bufferId: BufferId, row: Int, fromColumn: Int, toColumn: Option[Int])
  case BufferRows(bufferId: BufferId, rows: Set[Int])
  case PaneChrome(paneId: PaneId)
  case Surface(surfaceId: SurfaceId)

  /** Pixels outside every pane's own content -- the gutter and line numbers -- and nothing else. A change that also
    * recolors or reshapes pane content itself (a theme, a font, syntax highlighting) is `Everything`, not `Chrome`: a
    * consumer that reuses pane content pixels on `Chrome` alone would leave them stale for anything broader.
    */
  case Chrome
  case Everything
  case Combined(items: Set[Damage])

object Damage:

  given Monoid[Damage] with
    def empty: Damage = Nothing

    def combine(x: Damage, y: Damage): Damage =
      normalize(flatten(x) ++ flatten(y))

  /** The rows of `bufferId` that `damage` marks dirty. Coarsening cell-level damage to whole rows is a total function,
    * so this is the one place granularity gets thrown away -- callers that want cell precision read the `Damage` value
    * directly instead of going through this projection.
    *
    * Meaningless when [[isEverything]] holds for `damage` -- callers must check that first, since "every row" cannot be
    * expressed without knowing the buffer's current row count.
    */
  def coarsenToRows(bufferId: BufferId, damage: Damage): Set[Int] =
    flatten(damage).flatMap {
      case BufferRows(id, rows) if id == bufferId       => rows
      case BufferCells(id, row, _, _) if id == bufferId => Set(row)
      case _                                            => Set.empty[Int]
    }

  /** Whether `damage` requires a full repaint regardless of buffer, pane or surface -- the escape hatch for changes (a
    * resize, a config change touching every glyph) too broad to reason about per target.
    */
  def isEverything(damage: Damage): Boolean =
    flatten(damage).contains(Everything)

  /** Whether `damage` is expressed purely as per-buffer row/cell facts -- no `Chrome`, `PaneChrome`, `Surface` or
    * `Everything`. `Nothing` trivially qualifies (there is nothing to bound a repaint around, but nothing excluded is
    * present either). This is the shape a consumer must see before it can trust a screen repaint bounded to specific
    * pixel rects rather than the whole canvas: any of the excluded cases can touch pixels outside what per-buffer row
    * facts alone describe.
    */
  def isBufferRowsOnly(damage: Damage): Boolean =
    flatten(damage).forall {
      case BufferRows(_, _) | BufferCells(_, _, _, _) => true
      case _                                          => false
    }

  private def flatten(damage: Damage): Set[Damage] =
    damage match
      case Nothing         => Set.empty
      case Combined(items) => items.flatMap(flatten)
      case leaf            => Set(leaf)

  private def normalize(items: Set[Damage]): Damage =
    if items.contains(Everything) then Everything
    else
      val bufferRows: Map[BufferId, Set[Int]] =
        items
          .collect { case BufferRows(id, rows) => id -> rows }
          .groupMapReduce(_._1)(_._2)(_ ++ _)

      val bufferCells: Map[(BufferId, Int), (Int, Option[Int])] =
        items
          .collect { case BufferCells(id, row, from, to) => (id, row) -> (from, to) }
          .groupMapReduce(_._1)(_._2)(mergeSpans)

      val cellsNotSubsumedByRows =
        bufferCells.filterNot { case ((id, row), _) => bufferRows.get(id).exists(_.contains(row)) }

      val panes     = items.collect { case PaneChrome(id) => id }
      val surfaces  = items.collect { case Surface(id) => id }
      val hasChrome = items.contains(Chrome)

      val leaves: Set[Damage] =
        bufferRows.map { case (id, rows) => BufferRows(id, rows) }.toSet ++
          cellsNotSubsumedByRows.map { case ((id, row), (from, to)) => BufferCells(id, row, from, to) }.toSet ++
          panes.map(PaneChrome.apply) ++
          surfaces.map(Surface.apply) ++
          (if hasChrome then Set(Chrome) else Set.empty)

      leaves.headOption match
        case None                           => Nothing
        case Some(only) if leaves.size == 1 => only
        case Some(_)                        => Combined(leaves)

  private def mergeSpans(first: (Int, Option[Int]), second: (Int, Option[Int])): (Int, Option[Int]) =
    val from = math.min(first._1, second._1)
    val to = (first._2, second._2) match
      case (Some(firstTo), Some(secondTo)) => Some(math.max(firstTo, secondTo))
      case _                               => None
    (from, to)
