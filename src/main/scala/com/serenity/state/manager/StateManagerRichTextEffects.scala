package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.RichTextIntent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalStateReducer, RichTextReducer, RichTextRoute}
import com.serenity.ui.layout.PeekContent

/** Runs a rich-text formatting command as [[RichTextReducer.route]] decides for the active buffer: applies it, asks
  * before converting a plain-text file, or refuses it with a notice.
  */
final private[manager] class StateManagerRichTextEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit],
    showNotice: (PeekContent, CursorPosition) => IO[Unit]
):

  private[manager] def interpret(intent: RichTextIntent): IO[Unit] =
    currentState.flatMap { current =>
      RichTextReducer.route(intent, current) match
        case RichTextRoute.Apply =>
          val result = RichTextReducer.reduce(intent, current)
          commitState(result.state, current) >> result.effects.traverse_(interpretEffect)
        case RichTextRoute.AskToConvert(prompt) =>
          commitState(ModalStateReducer.show(Modal.Confirm(prompt), current).state, current)
        case RichTextRoute.Refuse(reason) =>
          showNotice(PeekContent.QuickInfo(reason), current.activeCursorPosition.getOrElse(CursorPosition(0, 0)))
    }
