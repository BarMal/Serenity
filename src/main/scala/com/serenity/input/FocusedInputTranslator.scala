package com.serenity.input

import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.*
import com.serenity.state.models.{AppState, SurfaceContent}

object FocusedInputTranslator:

  private val globalTranslator        = new GlobalHotkeyTranslator()
  private val editorTranslator        = new EditorInputTranslator()
  private val commandRunnerTranslator = new CommandRunnerTranslator()
  private val formTranslator          = new SingleLineFormTranslator()
  private val pinnedPanelTranslator   = new PinnedPanelTranslator()
  private val peekOverlayTranslator   = new PeekOverlayTranslator()

  def forState(state: AppState): Translator[Event] =
    val localTranslator =
      state.activeSurface match
        case Some(surface) =>
          surface.presentation match
            case com.serenity.state.models.SurfacePresentation.Pinned(_, _) =>
              pinnedPanelTranslator
            case _ =>
              surface.content match
                case SurfaceContent.CommandPalette(_)              => commandRunnerTranslator
                case SurfaceContent.CommandPaletteSubmenu(_, _, _) => commandRunnerTranslator
                case SurfaceContent.ModalWorkflow(_)               => formTranslator
                case SurfaceContent.ThemePicker(_)                 => formTranslator
                case SurfaceContent.FileSearch(_)                  => formTranslator
                case SurfaceContent.StartPage(_)                   => editorTranslator
                case _                                             => peekOverlayTranslator
        case None =>
          editorTranslator

    CompositeTranslator(globalTranslator, localTranslator)
