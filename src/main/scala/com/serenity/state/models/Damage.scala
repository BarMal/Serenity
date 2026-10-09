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

  /** Every row of one buffer. The same fact as `BufferRows(bufferId, (0 until lineCount).toSet)`, but its size does not
    * grow with the buffer, so a scroll or a reclassification of a 100k-line buffer costs no more than of a short one.
    */
  case BufferAll(bufferId: BufferId)
  case PaneChrome(paneId: PaneId)
  case Surface(surfaceId: SurfaceId)

  /** The pinned status row, and nothing else. Line numbers are not part of it: each is repainted with its own row, so
    * row damage carries them. A change that also recolors or reshapes pane content itself (a theme, a font, syntax
    * highlighting) is `Everything`, not `Chrome`: a consumer that reuses pane content pixels on `Chrome` alone would
    * leave them stale for anything broader.
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
    * Meaningless when [[isEverything]] or [[damagesEveryRow]] holds for `damage` -- callers must check those first,
    * since "every row" cannot be expressed without knowing the buffer's current row count.
    */
  def coarsenToRows(bufferId: BufferId, damage: Damage): Set[Int] =
    flatten(damage).flatMap {
      case BufferRows(id, rows) if id == bufferId       => rows
      case BufferCells(id, row, _, _) if id == bufferId => Set(row)
      case _                                            => Set.empty[Int]
    }

  /** What `damage` says about one buffer's rows: whether it names them all ([[BufferAll]]), the whole lines it names,
    * and the [[BufferCells]] spans kept apart from those, so a caller that knows how a line wraps can dirty only the
    * visual rows a span falls on.
    */
  final case class BufferRowFacts(everyRow: Boolean, lines: Set[Int], spans: List[BufferCells])

  def rowFactsFor(bufferId: BufferId, damage: Damage): BufferRowFacts =
    flatten(damage).foldLeft(BufferRowFacts(everyRow = false, Set.empty, Nil)) {
      case (facts, BufferAll(id)) if id == bufferId                    => facts.copy(everyRow = true)
      case (facts, BufferRows(id, rows)) if id == bufferId             => facts.copy(lines = facts.lines ++ rows)
      case (facts, cells @ BufferCells(id, _, _, _)) if id == bufferId => facts.copy(spans = cells :: facts.spans)
      case (facts, _)                                                  => facts
    }

  def damagesEveryRow(bufferId: BufferId, damage: Damage): Boolean =
    rowFactsFor(bufferId, damage).everyRow

  def damagedLines(bufferId: BufferId, damage: Damage): Set[Int] =
    rowFactsFor(bufferId, damage).lines

  def damagedSpans(bufferId: BufferId, damage: Damage): Set[BufferCells] =
    rowFactsFor(bufferId, damage).spans.toSet

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
      case BufferRows(_, _) | BufferCells(_, _, _, _) | BufferAll(_) => true
      case _                                                         => false
    }

  /** As [[isBufferRowsOnly]], also allowing [[Chrome]] and [[PaneChrome]]: the status row and each pane's header have a
    * known rect of their own, so they can join a bounded repaint rather than force the whole canvas.
    */
  def isBufferRowsOrChromeOnly(damage: Damage): Boolean =
    flatten(damage).forall {
      case BufferRows(_, _) | BufferCells(_, _, _, _) | BufferAll(_) | Chrome | PaneChrome(_) => true
      case _                                                                                  => false
    }

  def touchesChrome(damage: Damage): Boolean =
    flatten(damage).contains(Chrome)

  /** The panes whose header a [[PaneChrome]] fact anywhere in `damage` names. */
  def paneChromeIds(damage: Damage): Set[PaneId] =
    flatten(damage).collect { case PaneChrome(id) => id }

  /** The surfaces named by a `Surface(id)` fact anywhere in `damage`, flattening `Combined`. `Everything` reports none
    * here -- its lack of per-target detail is what `isEverything` is for, and a caller that needs to treat it as
    * touching every surface must check that separately.
    */
  def surfaceIds(damage: Damage): Set[SurfaceId] =
    flatten(damage).collect { case Surface(id) => id }

  /** Whether `damage` affects the independently-composited surface `surfaceId` -- `Everything` always does (it dirties
    * every target, surfaces included), a `Surface(surfaceId)` leaf does, and nothing else does (buffer/pane/chrome
    * damage has no bearing on a surface's own cached content). Used by a per-surface layer buffer -- see
    * `LayerCompositor`/`Renderer`'s modal layer -- to decide whether it may reuse what it last painted instead of
    * repainting.
    */
  def narrowToSurface(damage: Damage, surfaceId: SurfaceId): Damage =
    if isEverything(damage) then Everything
    else if flatten(damage).contains(Surface(surfaceId)) then Surface(surfaceId)
    else Nothing

  /** A `Combined` that `normalize` built already holds only leaves, so it is returned as is rather than rebuilt. */
  private def flatten(damage: Damage): Set[Damage] =
    damage match
      case Nothing                                 => Set.empty
      case Combined(items) if items.forall(isLeaf) => items
      case Combined(items)                         => items.flatMap(flatten)
      case leaf                                    => Set(leaf)

  private def isLeaf(damage: Damage): Boolean =
    damage match
      case Nothing | Combined(_) => false
      case _                     => true

  private def normalize(items: Set[Damage]): Damage =
    if items.contains(Everything) then Everything
    else
      val everyRow: Set[BufferId] = items.collect { case BufferAll(id) => id }

      val bufferRows: Map[BufferId, Set[Int]] =
        items.foldLeft(Map.empty[BufferId, Set[Int]]) {
          case (acc, BufferRows(id, rows)) if !everyRow.contains(id) =>
            acc.updated(id, acc.get(id).fold(rows)(_ ++ rows))
          case (acc, _) => acc
        }

      val cellsNotSubsumedByRows: Map[(BufferId, Int), List[BufferCells]] =
        items.foldLeft(Map.empty[(BufferId, Int), List[BufferCells]]) {
          case (acc, cells @ BufferCells(id, row, _, _))
              if !everyRow.contains(id) && !bufferRows.get(id).exists(_.contains(row)) =>
            acc.updated((id, row), cells :: acc.getOrElse((id, row), Nil))
          case (acc, _) => acc
        }

      val chromeAndSurfaces = items.filter {
        case PaneChrome(_) | Surface(_) | Chrome => true
        case _                                   => false
      }

      val leaves: Set[Damage] =
        chromeAndSurfaces ++
          everyRow.map(BufferAll.apply) ++
          bufferRows.map { case (id, rows) => BufferRows(id, rows) } ++
          cellsNotSubsumedByRows.valuesIterator.flatMap(mergeSpans)

      leaves.headOption match
        case None                           => Nothing
        case Some(only) if leaves.size == 1 => only
        case Some(_)                        => Combined(leaves)

  /** Overlapping or touching spans on one row merge; disjoint ones stay apart, so an old and a new caret on the same
    * long line stay two narrow facts instead of one span covering everything between them. Sorting first makes the
    * result depend only on the set of spans, which keeps `combine` associative. A row with one span, the usual case,
    * keeps it as is.
    */
  private def mergeSpans(spans: List[BufferCells]): List[Damage] =
    spans match
      case List(only) => List(only)
      case _ =>
        spans.sortBy(_.fromColumn).foldLeft(List.empty[BufferCells]) {
          case (last :: earlier, next) if last.toColumn.forall(_ >= next.fromColumn) =>
            val mergedTo = (last.toColumn, next.toColumn) match
              case (Some(lastEnd), Some(end)) => Some(math.max(lastEnd, end))
              case _                          => None
            last.copy(toColumn = mergedTo) :: earlier
          case (merged, next) => next :: merged
        }
