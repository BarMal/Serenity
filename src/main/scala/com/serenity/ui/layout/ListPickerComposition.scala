package com.serenity.ui.layout

import com.serenity.state.models.{ListChoice, ListPicker}
import com.serenity.ui.widget.{Loadable, TextField}

/** The one composition every [[ListPicker]] is drawn with: its title -- or what it is waiting on -- then its query if
  * it has one, then its choices, or a row saying they are loading, there are none, or why they couldn't be read. Under
  * choices its source can still add to, a muted row counts what is loaded.
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
    val queryRow       = picker.query.map(field => queryBox(field, ModalSurfaceComposition.rowRect(bounds, 1))).toList
    val firstChoiceRow = 1 + queryRow.size
    def messageRow(text: String, tone: OverlayTone) = List(
      ModalSurfaceComposition.textBox(text, ModalSurfaceComposition.rowRect(bounds, firstChoiceRow), tone = tone)
    )
    val rows = picker.items match
      case Loadable.Loading(_)   => messageRow("Loading…", OverlayTone.Muted)
      case Loadable.Empty(text)  => messageRow(text, OverlayTone.Muted)
      case Loadable.Failed(text) => messageRow(text, OverlayTone.Error)
      case Loadable.Ready(choices) =>
        val choiceRows = choices.visible(VisibleRows).toList.zipWithIndex.map {
          case ((choice, index), row) =>
            ModalSurfaceComposition.textBox(
              rowText(choice),
              ModalSurfaceComposition.rowRect(bounds, row + firstChoiceRow),
              selected = choices.selected.contains(index),
              focusId = Some(SurfaceFocusId(choiceActionId(index).value)),
              actionId = Some(choiceActionId(index))
            )
        }
        val moreRow = Option.when(picker.hasMore)(
          ModalSurfaceComposition.textBox(
            s"${choices.items.size} loaded, more available",
            ModalSurfaceComposition.rowRect(bounds, firstChoiceRow + choiceRows.size),
            tone = OverlayTone.Muted
          )
        )
        choiceRows ++ moreRow
    ModalSurfaceComposition.plan(bounds, header :: queryRow ++ rows)

  def frameHeight(picker: ListPicker): Int =
    val rows = picker.items match
      case Loadable.Ready(choices) => choices.items.size.min(VisibleRows) + (if picker.hasMore then 1 else 0)
      case _                       => 1
    SurfaceFrameLayout.DefaultBorderCells * 2 + 1 + picker.query.size + rows

  /** Label-less, and outside the hit regions: a click anywhere in the picker already leaves typing going here. */
  private def queryBox(field: TextField, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(
      SurfacePaintKind.TextInput,
      rect,
      text = Some(field.text),
      semanticLabel = Some(field.text),
      cursorOffset = Some(field.caret)
    )

  private def rowText(choice: ListChoice): String =
    choice.detail.fold(choice.label)(detail => s"${choice.label}  $detail")
