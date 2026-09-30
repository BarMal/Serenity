package com.serenity.ui.widget

/** What a dropdown reports back after an input, beyond its own new state. */
enum DropdownOutcome[+A]:
  case Chosen(item: A)
  case Dismissed

/** A closed field showing the chosen option, which opens into a list to pick another. With a `filter`, it is a combo
  * box: typing narrows the options by their label.
  *
  * Closed: Enter, Space, Down or a click opens it; Escape is left to whatever holds the dropdown. Open: the list takes
  * the arrows, Enter chooses, Escape closes it again without choosing.
  */
final case class Dropdown[A](
    options: Vector[A],
    label: A => String,
    chosen: Option[A] = None,
    open: Boolean = false,
    list: SelectableList[A] = SelectableList[A](Vector.empty),
    filter: Option[TextField] = None
):

  def shownOptions: Vector[A] =
    filter
      .filter(_.text.nonEmpty)
      .fold(options)(field => options.filter(option => label(option).toLowerCase.contains(field.text.toLowerCase)))

  def update(input: WidgetInput, visibleRows: Int): (Dropdown[A], Option[DropdownOutcome[A]]) =
    if open then updateOpen(input, visibleRows)
    else
      WidgetInput.asToggle(input) match
        case WidgetInput.Activate | WidgetInput.Toggle | WidgetInput.Down | WidgetInput.Click(_, _) =>
          (opened(visibleRows), None)
        case _ => (this, None)

  def opened(visibleRows: Int): Dropdown[A] =
    val reset = copy(open = true, filter = filter.map(_ => TextField()))
    val shown = reset.shownOptions
    val start = chosen.map(shown.indexOf).filter(_ >= 0).getOrElse(0)
    reset.copy(list = SelectableList(shown).select(start, visibleRows))

  def closed: Dropdown[A] = copy(open = false)

  private def updateOpen(input: WidgetInput, visibleRows: Int): (Dropdown[A], Option[DropdownOutcome[A]]) =
    input match
      case WidgetInput.Dismiss => (closed, Some(DropdownOutcome.Dismissed))
      case typing @ (WidgetInput.Insert(_) | WidgetInput.InsertText(_) | WidgetInput.DeleteBackward |
          WidgetInput.DeleteForward | WidgetInput.DeleteWordBackward | WidgetInput.DeleteWordForward)
          if filter.isDefined =>
        val refiltered = copy(filter = filter.map(_.update(typing)._1))
        (refiltered.copy(list = list.withItems(refiltered.shownOptions, visibleRows)(_ == _)), None)
      case other =>
        list.update(other, visibleRows) match
          case (_, Some(ListOutcome.Activated(_, item))) =>
            (copy(chosen = Some(item), open = false), Some(DropdownOutcome.Chosen(item)))
          case (_, Some(ListOutcome.Dismissed)) => (closed, Some(DropdownOutcome.Dismissed))
          case (moved, None)                    => (copy(list = moved), None)

object Dropdown:

  def of[A](options: Seq[A], label: A => String, chosen: Option[A] = None, filterable: Boolean = false): Dropdown[A] =
    Dropdown(options.toVector, label, chosen, filter = Option.when(filterable)(TextField()))
