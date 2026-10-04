package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.RichTextIntent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalStateReducer, ReducerResult, RichTextReducer, RichTextRoute}
import com.serenity.ui.layout.PeekContent

/** Runs a rich-text formatting command as [[RichTextReducer.route]] decides for the active buffer: applies it, asks
  * before converting a plain-text file, or refuses it with a notice. An applied command commits its undo boundary in
  * the same model write as the formatting it undoes.
  */
final private[manager] class StateManagerRichTextEffects(
    currentState: IO[AppState],
    updateModelValidated: (Model => Option[Model]) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit],
    showNotice: (PeekContent, CursorPosition) => IO[Unit]
):

  private[manager] def interpret(intent: RichTextIntent): IO[Unit] =
    currentState.flatMap { current =>
      RichTextReducer.route(intent, current) match
        case RichTextRoute.Apply =>
          val result = RichTextReducer.reduce(intent, current)
          updateModelValidated(withResult(result)) >>
            result.effects.filterNot(ModelCommit.isModelEffect).traverse_(interpretEffect)
        case RichTextRoute.AskToConvert(prompt) =>
          val asked = ModalStateReducer.show(Modal.Confirm(prompt), current).state
          updateModelValidated(model => Some(model.copy(app = asked)))
        case RichTextRoute.Refuse(reason) =>
          showNotice(PeekContent.QuickInfo(reason), current.activeCursorPosition.getOrElse(CursorPosition(0, 0)))
    }

  private def withResult(result: ReducerResult)(model: Model): Option[Model] =
    Some(ModelCommit.applyModelEffects(model.copy(app = result.state), result.effects))
