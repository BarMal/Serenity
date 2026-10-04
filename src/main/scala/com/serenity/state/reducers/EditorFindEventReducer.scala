package com.serenity.state.reducers

import com.serenity.keystroke.events.*
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
        ModalStateReducer.show(Modal.TextPrompt(TextPrompt.gotoLine()), currentState)

      case OpenFind =>
        ModalStateReducer.show(findModalForBuffer(buffer), currentState)

      case OpenReplace =>
        ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), currentState)

      case FindNext =>
        buffer.findState match
          case Some(FindState(query, storedResults, currentIndex)) if storedResults.nonEmpty =>
            val content = buffer.document.content
            val resultSet =
              if storedResults.forall(FindSearch.stillMatches(content, query, _)) then
                FindResultSet.normalized(query, storedResults, currentIndex + 1)
              else
                val refreshed = FindSearch.results(content, query)
                FindResultSet.normalized(
                  query,
                  refreshed,
                  FindResultSet.indexAfter(refreshed, buffer.editing.cursors.head.position)
                )
            if resultSet.results.isEmpty then applyBuffer(_.copy(findState = None))
            else
              val selected = resultSet.results(resultSet.currentIndex)
              val target   = CursorPosition(selected.line, selected.column)
              applyBuffer(
                _.copy(
                  editing = EditingState(List(target)),
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
        Modal.Find("", Vector.empty, 0)
