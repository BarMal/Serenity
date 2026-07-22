package com.serenity.input

import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.*
import com.serenity.state.models.{AppState, SurfaceContent}

object FocusedInputTranslator:

  def forState(state: AppState): Translator[Event] =
    val editorTranslator        = new EditorInputTranslator(state.config)
    val commandRunnerTranslator = new CommandRunnerTranslator(state.config)
    val formTranslator          = new SingleLineFormTranslator(state.config)
    val pinnedPanelTranslator   = new PinnedPanelTranslator(state.config)
    val peekOverlayTranslator   = new PeekOverlayTranslator(state.config)
    val localTranslator =
      if state.hasCommandRunnerDomain then commandRunnerTranslator
      else
        state.activeSurface match
          case Some(surface) =>
            surface.presentation match
              case com.serenity.state.models.SurfacePresentation.Pinned(_, _) =>
                pinnedPanelTranslator
              case com.serenity.state.models.SurfacePresentation.Expanded(_, _) =>
                pinnedPanelTranslator
              case _ =>
                surface.content match
                  case SurfaceContent.CommandPalette(_)              => commandRunnerTranslator
                  case SurfaceContent.CommandPaletteSubmenu(_, _, _) => commandRunnerTranslator
                  case SurfaceContent.ModalWorkflow(_)               => formTranslator
                  case SurfaceContent.ThemePicker(_)                 => formTranslator
                  case SurfaceContent.ThemeCreator(_)                => formTranslator
                  case SurfaceContent.FileSearch(_)                  => formTranslator
                  case SurfaceContent.ContextualToolbar(_)           => formTranslator
                  case SurfaceContent.CommentLens(_)                 => formTranslator
                  case SurfaceContent.StartPage(_)                   => editorTranslator
                  case _                                             => peekOverlayTranslator
          case None =>
            editorTranslator

    if state.hasBlockingModal then formTranslator
    else CompositeTranslator(new GlobalHotkeyTranslator(state.config), localTranslator)
