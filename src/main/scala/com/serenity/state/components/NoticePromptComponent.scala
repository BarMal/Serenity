package com.serenity.state.components

import com.serenity.keystroke.events.{Enter, Escape, Event, MoveLeft, MoveRight, NewLine, ReverseTabKey, TabKey}
import com.serenity.state.models.{AppState, NoticePrompt, SurfaceContent}
import com.serenity.state.reducers.NoticeReducer

/** Keyboard handling for a notice that asks a question: Left/Right or Tab/Shift+Tab move the highlight, Enter chooses
  * the highlighted action and Escape dismisses the question. Every other key goes on to the editor, so typing is not
  * interrupted -- and so is Enter while nothing is highlighted, since an Enter meant for the text must not answer for
  * the user.
  */
class NoticePromptComponent extends TypedFocusedComponent[Event]:

  protected def decodeEvent(event: Event): Option[Event] = Some(event)

  protected def processTypedEvent(event: Event, currentState: AppState): ComponentResult =
    focusedPrompt(currentState).fold(ComponentResult.unhandled) { prompt =>
      event match
        case MoveRight | TabKey       => ComponentResult.updateState(NoticeReducer.highlighted(_, prompt.id, 1))
        case MoveLeft | ReverseTabKey => ComponentResult.updateState(NoticeReducer.highlighted(_, prompt.id, -1))
        case Enter | NewLine =>
          prompt.highlighted.fold(ComponentResult.unhandled) { index =>
            ComponentResult.reducerResult(NoticeReducer.answered(currentState, prompt.id, Some(index)))
          }
        case Escape => ComponentResult.reducerResult(NoticeReducer.answered(currentState, prompt.id, None))
        case _      => ComponentResult.unhandled
    }

  private def focusedPrompt(state: AppState): Option[NoticePrompt] =
    state.activeSurface.flatMap { surface =>
      surface.content match
        case SurfaceContent.Notice(notice, _) => notice.prompt
        case _                                => None
    }
