package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.widget.TextField

/** Goto-line/find/replace modal opening and find-next/previous -- the family that reads or advances find state rather
  * than editing the document. Split out of `EditorEventReducer.reduceCursorsEditEvent`'s sibling dispatch when that
  * file grew past its 600-line target.
  */
private[reducers] object EditorFindEventReducer:

  def reduce(event: TextEntryEvent, ctx: EditorCursorSupport.CursorEventContext): ReducerResult =
    import ctx.*

    def applyBuffer(f: Buffer => Buffer): ReducerResult =
      ReducerResult.noEffects(Focused.replaceBuffer(currentState, f(buffer)))

    def stepped(direction: FindDirection): ReducerResult =
      buffer.findState match
        case Some(findState) if findState.results.nonEmpty =>
          val caret = buffer.editing.cursors.head.position
          FindNavigation
            .step(buffer.document.content, findState, caret, direction)
            .flatMap(next => next.resultSet.selectedResult.map(next -> _)) match
            case Some((next, selected)) =>
              applyBuffer(
                _.copy(
                  editing = EditingState(List(CursorPosition(selected.line, selected.column))),
                  findState = Some(next)
                )
              )
            case None => applyBuffer(_.copy(findState = None))
        case _ =>
          ReducerResult.noEffects(currentState)

    event match
      case OpenGotoLine =>
        ModalStateReducer.show(Modal.TextPrompt(TextPrompt.gotoLine()), currentState)

      case OpenFind =>
        ModalStateReducer.show(findModalForBuffer(buffer), currentState)

      case OpenReplace =>
        ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), currentState)

      case FindNext     => stepped(FindDirection.Forward)
      case FindPrevious => stepped(FindDirection.Backward)

      case _ =>
        ReducerResult.noEffects(currentState)

  private def findModalForBuffer(buffer: Buffer): Modal =
    buffer.findState match
      case Some(found) if found.query.nonEmpty =>
        val resultSet = found.resultSet
        Modal.Find(
          TextField.of(resultSet.query),
          resultSet.results,
          resultSet.currentIndex,
          found.options,
          resultSet.capped
        )
      case _ =>
        Modal.Find(TextField(), Vector.empty, 0)
