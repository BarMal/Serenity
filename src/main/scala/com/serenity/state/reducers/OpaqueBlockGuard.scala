package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** A block line (a table kept read-only) holds nothing but its block. An edit is refused when it leaves other content
  * beside a block atom; one that removes the atom, and with it the block, is the user taking the block out.
  *
  * The check runs after the edit and only when a cursor or selection is on or next to a block line, so typing away from
  * blocks costs a lookup of the lines around the cursors and nothing more.
  */
private[reducers] object OpaqueBlockGuard:

  def refuses(event: TextEntryEvent, before: Buffer, after: AppState): Boolean =
    editsText(event) && nextToBlock(before) &&
      after.persisted.buffers.get(before.id).flatMap(_.richText.richTextDocument).exists(_.hasMixedBlock)

  private def editsText(event: TextEntryEvent): Boolean =
    event match
      case InsertChar(_) | TabKey | ReverseTabKey | NewLine | Enter | DeleteBackward | DeleteForward |
          DeleteWordBackward | DeleteWordForward | Cut | CutToDarlings | RestoreDarling | Paste | PasteFromHistory(_) =>
        true
      case _ => false

  /** Whether a block line is among the lines the cursors and their selections cover, or the lines either side of them,
    * which an edit at a line's start or end joins to.
    */
  private def nextToBlock(buffer: Buffer): Boolean =
    buffer.richText.richTextDocument.filter(_ => buffer.richTextInSync).exists { document =>
      val lines =
        buffer.cursorList.toList.flatMap(cursor => cursor.position.line :: cursor.selectionAnchor.map(_.line).toList)
      val (first, last) =
        lines.foldLeft((Int.MaxValue, Int.MinValue))((bounds, line) => (bounds._1.min(line), bounds._2.max(line)))
      document.hasOpaqueBlockBetween((first - 1).max(0), last + 1)
    }
