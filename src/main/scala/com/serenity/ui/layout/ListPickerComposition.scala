package com.serenity.ui.layout

import com.serenity.state.models.{ListChoice, ListPicker}
import com.serenity.ui.widget.Loadable

/** The one composition every [[ListPicker]] is drawn with: its title -- or what it is waiting on -- then its choices,
  * or a row saying they are loading, there are none, or why they couldn't be read.
  */
object ListPickerComposition:

  /** At most this many choices show at once; the list scrolls to keep the highlighted one among them. */
  val VisibleRows: Int = 8

  private val ActionIdPrefix = "list-choice-"

  def choiceActionId(index: Int): SurfaceActionId = SurfaceActionId(s"$ActionIdPrefix$index")

  /** The choice a hit region's action id names -- only ever one [[choiceActionId]] produced. */
  def choiceIndex(actionId: String): Option[Int] =
    Option.when(actionId.startsWith(ActionIdPrefix))(actionId.stripPrefix(ActionIdPrefix)).flatMap(_.toIntOption)

  def forPicker(picker: ListPicker, frameRect: LayoutRect): ResolvedSurfaceComposition =
    val content = SurfaceFrameLayout(frameRect).contentRect
    val bounds  = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val header = ModalSurfaceComposition.textBox(
      picker.pending.flatMap(_.waitingLabel).getOrElse(picker.title),
      ModalSurfaceComposition.rowRect(bounds, 0)
    )
    def messageRow(text: String, tone: OverlayTone) = List(
      ModalSurfaceComposition.textBox(text, ModalSurfaceComposition.rowRect(bounds, 1), tone = tone)
    )
    val rows = picker.items match
      case Loadable.Loading(_)   => messageRow("Loading…", OverlayTone.Muted)
      case Loadable.Empty(text)  => messageRow(text, OverlayTone.Muted)
      case Loadable.Failed(text) => messageRow(text, OverlayTone.Error)
      case Loadable.Ready(choices) =>
        choices.visible(VisibleRows).toList.zipWithIndex.map {
          case ((choice, index), row) =>
            ModalSurfaceComposition.textBox(
              rowText(choice),
              ModalSurfaceComposition.rowRect(bounds, row + 1),
              selected = choices.selected.contains(index),
              focusId = Some(SurfaceFocusId(choiceActionId(index).value)),
              actionId = Some(choiceActionId(index))
            )
        }
    ModalSurfaceComposition.plan(bounds, header :: rows)

  def frameHeight(picker: ListPicker): Int =
    val rows = picker.items match
      case Loadable.Ready(choices) => choices.items.size.min(VisibleRows)
      case _                       => 1
    SurfaceFrameLayout.DefaultBorderCells * 2 + 1 + rows

  private def rowText(choice: ListChoice): String =
    choice.detail.fold(choice.label)(detail => s"${choice.label}  $detail")
