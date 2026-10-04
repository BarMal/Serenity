package com.serenity.ui.widget

/** What moving past either end of a list does. */
enum EndBehaviour:
  case Wrap
  case Stop

/** What a list reports back after an input, beyond its own new state. */
enum ListOutcome[+A]:
  case Activated(index: Int, item: A)
  case Dismissed

/** A list with a selection and a scroll position, the shared model behind menus, pickers, panel lists, dropdowns, tabs
  * and trees.
  *
  * The scroll `offset` is stored, not derived from the selection, so the wheel can scroll the list without moving the
  * selection; any move of the selection scrolls it back into view. `visibleRows` is the viewport height the renderer
  * measured: paging and scroll-into-view need it, and nothing else here knows it.
  */
final case class SelectableList[+A](
    items: Vector[A],
    selected: Option[Int] = None,
    offset: Int = 0,
    hovered: Option[Int] = None,
    endBehaviour: EndBehaviour = EndBehaviour.Wrap
):

  def isEmpty: Boolean = items.isEmpty

  def selectedItem: Option[A] = selected.flatMap(items.lift)

  /** The items the viewport shows, each with its index. */
  def visible(visibleRows: Int): Vector[(A, Int)] =
    items.slice(offset, offset + math.max(1, visibleRows)).zip(Iterator.from(offset.max(0)))

  def update(input: WidgetInput, visibleRows: Int): (SelectableList[A], Option[ListOutcome[A]]) =
    input match
      case WidgetInput.Up            => (moveBy(-1, visibleRows), None)
      case WidgetInput.Down          => (moveBy(1, visibleRows), None)
      case WidgetInput.PageUp        => (pageBy(-1, visibleRows), None)
      case WidgetInput.PageDown      => (pageBy(1, visibleRows), None)
      case WidgetInput.First         => (select(0, visibleRows), None)
      case WidgetInput.Last          => (select(items.size - 1, visibleRows), None)
      case WidgetInput.Activate      => (this, activated)
      case WidgetInput.Dismiss       => (this, Some(ListOutcome.Dismissed))
      case WidgetInput.Scroll(lines) => (scrollBy(lines, visibleRows), None)
      case WidgetInput.Hover(index)  => (copy(hovered = index.filter(items.indices.contains)), None)
      case WidgetInput.Click(index, clicks) =>
        val clicked = select(index, visibleRows)
        (clicked, if clicks >= 2 && clicked.selected.contains(index) then clicked.activated else None)
      case _ => (this, None)

  def select(index: Int, visibleRows: Int): SelectableList[A] =
    if items.isEmpty then copy(selected = None, offset = 0)
    else copy(selected = Some(index.max(0).min(items.size - 1))).scrolledToSelection(visibleRows)

  def moveBy(delta: Int, visibleRows: Int): SelectableList[A] =
    if items.isEmpty then this
    else
      val start = selected.getOrElse(if delta > 0 then -1 else items.size)
      val next = endBehaviour match
        case EndBehaviour.Wrap => Math.floorMod(start + delta, items.size)
        case EndBehaviour.Stop => (start + delta).max(0).min(items.size - 1)
      select(next, visibleRows)

  /** Paging never wraps: a page past the end lands on the last item. */
  def pageBy(pages: Int, visibleRows: Int): SelectableList[A] =
    if items.isEmpty then this
    else select(selected.getOrElse(0) + pages * math.max(1, visibleRows - 1), visibleRows)

  def scrollBy(lines: Int, visibleRows: Int): SelectableList[A] =
    copy(offset = clampOffset(offset + lines, visibleRows))

  /** Replaces the items, keeping the selection on the same item when it is still there (matched by `sameItem`), and
    * otherwise on the same position.
    */
  def withItems[B](newItems: Vector[B], visibleRows: Int)(sameItem: (A, B) => Boolean): SelectableList[B] =
    val kept = selectedItem.flatMap(item =>
      newItems.indexWhere(sameItem(item, _)) match
        case -1    => None
        case index => Some(index)
    )
    val reselected = kept.orElse(selected.filter(_ => newItems.nonEmpty).map(_.min(newItems.size - 1)))
    SelectableList(newItems, reselected, offset, None, endBehaviour).scrolledToSelection(visibleRows)

  private def activated: Option[ListOutcome[A]] =
    for
      index <- selected
      item  <- items.lift(index)
    yield ListOutcome.Activated(index, item)

  private def scrolledToSelection(visibleRows: Int): SelectableList[A] =
    val rows = math.max(1, visibleRows)
    selected.fold(copy(offset = clampOffset(offset, rows))) { index =>
      val intoView =
        if index < offset then index
        else if index >= offset + rows then index - rows + 1
        else offset
      copy(offset = clampOffset(intoView, rows))
    }

  private def clampOffset(candidate: Int, visibleRows: Int): Int =
    candidate.min(items.size - math.max(1, visibleRows)).max(0)

object SelectableList:

  def of[A](items: Seq[A], endBehaviour: EndBehaviour = EndBehaviour.Wrap): SelectableList[A] =
    SelectableList(items.toVector, Option.when(items.nonEmpty)(0), endBehaviour = endBehaviour)
