package com.serenity.ui.layout

/** The width a dialog's prose wraps to, and how it is measured. Sizing and painting both build one from the same frame
  * width, so the rows a frame is sized for are the rows it paints.
  */
final case class ModalWrap(widthCells: Int, measure: TextMeasure):

  def lines(text: String): List[String] = TextWrapping.wrap(text, widthCells, measure)

  def lines(texts: List[String]): List[String] = texts.flatMap(lines)

object ModalWrap:

  def ofContent(widthCells: Int, measure: TextMeasure = TextMeasure.Cells): ModalWrap =
    ModalWrap(math.max(1, widthCells), measure)

  def ofFrame(frameWidth: Int, measure: TextMeasure = TextMeasure.Cells): ModalWrap =
    ofContent(frameWidth - 2 * SurfaceFrameLayout.DefaultBorderCells, measure)
