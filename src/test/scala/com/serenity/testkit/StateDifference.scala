package com.serenity.testkit

import com.serenity.state.models.{Cursor, EditingState, Viewport}

/** What differs between the editor a batch was applied to and the one its events were applied to one at a time, for the
  * message of a spec that compares them: a failure that names only the slice cannot say whether the text, the cursors,
  * the viewport or the undo history diverged.
  */
object StateDifference:

  type Shown = (String, EditingState, Viewport)

  def describe(
    perEvent: Option[Shown],
    batched: Option[Shown],
    perEventDepth: Int,
    batchedDepth: Int
  ): List[String] =
    val buffers = (perEvent, batched) match
      case (Some((text, cursors, viewport)), Some((batchedText, batchedCursors, batchedViewport))) =>
        List(
          textDifference(text, batchedText),
          Option.when(cursors != batchedCursors)(
            s"cursors differ: one at a time ${editing(cursors, batchedCursors)._1}, batched ${editing(cursors, batchedCursors)._2}"
          ),
          Option.when(viewport != batchedViewport)(
            s"viewport differs: one at a time ${show(viewport)}, batched ${show(batchedViewport)}"
          )
        ).flatten
      case (Some(_), None) => List("the batched editor has no buffer")
      case (None, Some(_)) => List("the one-at-a-time editor has no buffer")
      case (None, None)    => Nil
    buffers ++ Option.when(perEventDepth != batchedDepth)(
      s"undo depth: $perEventDepth one event at a time, $batchedDepth batched"
    )

  private def textDifference(expected: String, actual: String): Option[String] =
    Option.when(expected != actual) {
      val at = expected.zip(actual).indexWhere((a, b) => a != b) match
        case -1    => math.min(expected.length, actual.length)
        case found => found
      def charAt(text: String) = if at < text.length then s"'${text.charAt(at)}'" else "the end"
      s"text differs at offset $at (expected ${charAt(expected)}, actual ${charAt(actual)}; lengths ${expected.length} and ${actual.length})"
    }

  /** Both sides in the same words; when those match, the whole state, so that a difference in a field the words leave
    * out is still shown.
    */
  private def editing(expected: EditingState, actual: EditingState): (String, String) =
    val (a, b) = (cursors(expected), cursors(actual))
    if a == b then (expected.toString, actual.toString) else (a, b)

  private def cursors(state: EditingState): String =
    state.cursors.toList.map(cursor).mkString("[", ", ", "]")

  private def cursor(cursor: Cursor): String =
    val anchor = cursor.selectionAnchor.map(a => s" anchor=${a.line}:${a.column}").getOrElse("")
    s"line=${cursor.position.line} column=${cursor.position.column}$anchor"

  private def show(viewport: Viewport): String =
    s"topLine=${viewport.topLine} leftColumn=${viewport.leftColumn} topVisualLine=${viewport.topVisualLine} " +
      s"visible=${viewport.visibleLines}x${viewport.visibleColumns} placement=${viewport.placement}"
