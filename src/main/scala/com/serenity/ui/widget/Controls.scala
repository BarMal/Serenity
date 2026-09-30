package com.serenity.ui.widget

/** How much a button stands out: the one primary action, ordinary ones, and ones that destroy something. */
enum ButtonEmphasis:
  case Primary
  case Secondary
  case Danger

/** A labelled action. A disabled button still shows, and still takes focus so its `disabledReason` can be read out, but
  * never activates.
  */
final case class Button[+A](
    label: String,
    action: A,
    emphasis: ButtonEmphasis = ButtonEmphasis.Secondary,
    disabledReason: Option[String] = None
):
  def enabled: Boolean = disabledReason.isEmpty

  /** Enter, Space or a click on an enabled button. */
  def pressed(input: WidgetInput): Option[A] =
    WidgetInput.asToggle(input) match
      case WidgetInput.Activate | WidgetInput.Toggle | WidgetInput.Click(_, _) if enabled => Some(action)
      case _                                                                              => None

/** A labelled on/off setting, drawn as a checkbox or a switch. */
final case class Checkbox(label: String, checked: Boolean, disabledReason: Option[String] = None):
  def enabled: Boolean = disabledReason.isEmpty

  def update(input: WidgetInput): Checkbox =
    WidgetInput.asToggle(input) match
      case WidgetInput.Toggle | WidgetInput.Activate | WidgetInput.Click(_, _) if enabled => copy(checked = !checked)
      case _                                                                              => this

/** One choice from a few, all on show: the arrows move the highlight, Space or Enter chooses it. */
final case class RadioGroup[A](list: SelectableList[A], chosen: Option[A]):

  def update(input: WidgetInput, visibleRows: Int): (RadioGroup[A], Option[A]) =
    WidgetInput.asToggle(input) match
      case WidgetInput.Toggle =>
        list.selectedItem.fold((this, None))(item => (copy(chosen = Some(item)), Some(item)))
      case WidgetInput.Click(index, _) =>
        val clicked = list.select(index, visibleRows)
        clicked.selectedItem.fold((copy(list = clicked), None))(item => (RadioGroup(clicked, Some(item)), Some(item)))
      case other =>
        list.update(other, visibleRows) match
          case (moved, Some(ListOutcome.Activated(_, item))) => (RadioGroup(moved, Some(item)), Some(item))
          case (moved, _)                                    => (copy(list = moved), None)

object RadioGroup:

  def of[A](options: Seq[A], chosen: Option[A]): RadioGroup[A] =
    val list = SelectableList.of(options, EndBehaviour.Stop)
    RadioGroup(chosen.map(options.indexOf).filter(_ >= 0).fold(list)(list.select(_, options.size)), chosen)

/** A row of tabs: Left and Right move between them and the selected tab is the active one, so there is no separate
  * activation step.
  */
final case class Tabs[A](list: SelectableList[A]):

  def active: Option[A] = list.selectedItem

  def update(input: WidgetInput): (Tabs[A], Option[A]) =
    val moved = input match
      case WidgetInput.Left            => list.moveBy(-1, list.items.size)
      case WidgetInput.Right           => list.moveBy(1, list.items.size)
      case WidgetInput.First           => list.select(0, list.items.size)
      case WidgetInput.Last            => list.select(list.items.size - 1, list.items.size)
      case WidgetInput.Click(index, _) => list.select(index, list.items.size)
      case WidgetInput.Hover(index)    => list.update(input, list.items.size)._1
      case _                           => list
    (Tabs(moved), moved.selectedItem.filter(_ => moved.selected != list.selected))

object Tabs:
  def of[A](tabs: Seq[A]): Tabs[A] = Tabs(SelectableList.of(tabs))

/** Which of a set of widgets has focus, moved by Tab and Shift+Tab, wrapping at either end. */
final case class FocusRing[F](order: Vector[F], focused: Option[F]):

  def next: FocusRing[F]     = step(1)
  def previous: FocusRing[F] = step(-1)

  def focus(target: F): FocusRing[F] = if order.contains(target) then copy(focused = Some(target)) else this

  private def step(delta: Int): FocusRing[F] =
    if order.isEmpty then this
    else
      val current = focused.map(order.indexOf).filter(_ >= 0).getOrElse(if delta > 0 then -1 else 0)
      copy(focused = Some(order(Math.floorMod(current + delta, order.size))))

object FocusRing:
  def of[F](order: Seq[F]): FocusRing[F] = FocusRing(order.toVector, order.headOption)
