package com.serenity.ui.widget

/** What a multi-select list reports back after an input, beyond its own new state. */
enum MultiSelectOutcome[+A]:
  case Confirmed(checked: Vector[A])
  case Dismissed

/** A list whose items can each be checked, as well as selected: Space toggles the selected item, Enter confirms the
  * checked set. Checks are kept by item, so they survive the items being reordered or filtered.
  */
final case class MultiSelectList[A](list: SelectableList[A], checked: Set[A] = Set.empty[A]):

  def isChecked(item: A): Boolean = checked.contains(item)

  /** The checked items, in list order. */
  def checkedItems: Vector[A] = list.items.filter(checked.contains)

  def update(input: WidgetInput, visibleRows: Int): (MultiSelectList[A], Option[MultiSelectOutcome[A]]) =
    WidgetInput.asToggle(input) match
      case WidgetInput.Toggle    => (list.selectedItem.fold(this)(toggled), None)
      case WidgetInput.SelectAll => (checkAll, None)
      case WidgetInput.Activate  => (this, Some(MultiSelectOutcome.Confirmed(checkedItems)))
      case WidgetInput.Click(index, _) =>
        val clicked = copy(list = list.select(index, visibleRows))
        (clicked.list.selectedItem.fold(clicked)(clicked.toggled), None)
      case other =>
        val (moved, outcome) = list.update(other, visibleRows)
        (copy(list = moved), outcome.collect { case ListOutcome.Dismissed => MultiSelectOutcome.Dismissed })

  def toggled(item: A): MultiSelectList[A] =
    copy(checked = if checked.contains(item) then checked - item else checked + item)

  /** Checks every item, or clears them all when every one is already checked. */
  def checkAll: MultiSelectList[A] =
    copy(checked = if list.items.forall(checked.contains) then Set.empty else list.items.toSet)

object MultiSelectList:

  def of[A](items: Seq[A], checked: Set[A] = Set.empty[A]): MultiSelectList[A] =
    MultiSelectList(SelectableList.of(items, EndBehaviour.Stop), checked)
