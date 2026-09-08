package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Goto-line/find/replace modal opening and find-next -- the family that reads or advances find state rather than
  * editing the document. Split out of `EditorEventReducer.reduceCursorsEditEvent`'s sibling dispatch when that file
  * grew past its 600-line target.
  */
private[reducers] object EditorFindEventReducer:

  def reduce(event: TextEntryEvent, ctx: EditorCursorSupport.CursorEventContext): ReducerResult =
    import ctx.*

    def applyBuffer(f: Buffer => Buffer): ReducerResult =
      ReducerResult.noEffects(Focused.replaceBuffer(currentState, f(buffer)))

    event match
      case OpenGotoLine =>
        ModalStateReducer.show(Modal.GotoLine(""), currentState)

      case OpenFind =>
        ModalStateReducer.show(findModalForBuffer(buffer), currentState)

      case OpenReplace =>
        ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), currentState)

      case FindNext =>
        buffer.findState match
          case Some(FindState(query, storedResults, currentIndex)) if storedResults.nonEmpty =>
            val validResults = storedResults.filter { result =>
              isWholeGraphemeMatch(
                buffer.document.content,
                buffer.document.content.lineColumnToOffset(result.line, result.column),
                query.length
              )
            }
            val resultSet = FindResultSet.normalized(query, validResults, currentIndex + 1)
            if resultSet.results.isEmpty then applyBuffer(_.copy(findState = None))
            else
              val selected = resultSet.results(resultSet.currentIndex)
              val target   = CursorPosition(selected.line, selected.column)
              applyBuffer(
                _.copy(
                  editing = buffer.editing.copy(
                    cursors = List(target),
                    selection = None,
                    selections = Nil,
                    preferredColumn = Some(target.column),
                    preferredXPx = None
                  ),
                  findState = Some(FindState.fromResultSet(resultSet))
                )
              )
          case _ =>
            ReducerResult.noEffects(currentState)

      case _ =>
        ReducerResult.noEffects(currentState)

  private def findModalForBuffer(buffer: Buffer): Modal =
    buffer.findState match
      case Some(FindState(query, results, currentIndex)) if query.nonEmpty =>
        val resultSet = FindResultSet.normalized(query, results, currentIndex)
        Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
      case _ =>
        Modal.Find("", Nil, 0)

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)
