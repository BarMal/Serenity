package com.serenity.input

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.*
import com.serenity.state.models.{AppState, SurfaceContent}

object FocusedInputTranslator:

  /** The fixed set of per-focus translators, built once per distinct [[AppConfig]] identity rather than on every
    * input event -- construction is what flattens each translator's local keymap, the expensive part (issue #1409).
    * `forState` picks one of these (or composes it with the global hotkey translator) based on the rest of the
    * `AppState`, which changes far more often than the config does.
    */
  final case class TranslatorSet(
      editor: EditorInputTranslator,
      commandRunner: CommandRunnerTranslator,
      form: SingleLineFormTranslator,
      pinnedPanel: PinnedPanelTranslator,
      peekOverlay: PeekOverlayTranslator,
      globalHotkey: GlobalHotkeyTranslator
  )

  object TranslatorSet:
    def forConfig(config: AppConfig): TranslatorSet =
      TranslatorSet(
        new EditorInputTranslator(config),
        new CommandRunnerTranslator(config),
        new SingleLineFormTranslator(config),
        new PinnedPanelTranslator(config),
        new PeekOverlayTranslator(config),
        new GlobalHotkeyTranslator(config)
      )

  /** The one place the real clock is sampled; the overload below takes it explicitly. */
  def forState(state: AppState): Translator[Event] =
    forState(state, () => System.currentTimeMillis())

  def forState(state: AppState, now: () => Long): Translator[Event] =
    forState(state, TranslatorSet.forConfig(state.persisted.config), now)

  /** As the one-arg overload, but reuses a [[TranslatorSet]] already built for `state.persisted.config` -- the entry
    * point for a caller (e.g. `AppRuntime`) keeping one cached per config identity instead of rebuilding it on every
    * call.
    */
  def forState(state: AppState, translators: TranslatorSet): Translator[Event] =
    forState(state, translators, () => System.currentTimeMillis())

  def forState(state: AppState, translators: TranslatorSet, now: () => Long): Translator[Event] =
    val recordingBinding = state.activeSurface.exists { surface =>
      surface.content match
        case SurfaceContent.CommandPalette(runner) =>
          runner.recordingItemId.nonEmpty || runner.activeSettingsSurface.exists(_.current.recording.nonEmpty)
        case _ => false
    }
    val localTranslator =
      if state.hasCommandRunnerDomain then translators.commandRunner
      else
        state.activeSurface match
          case Some(surface) =>
            surface.presentation match
              case com.serenity.state.models.SurfacePresentation.Docked =>
                translators.pinnedPanel
              case _ =>
                surface.content match
                  case SurfaceContent.CommandPalette(_)    => translators.commandRunner
                  case SurfaceContent.ModalWorkflow(_)     => translators.form
                  case SurfaceContent.ThemePicker(_)       => translators.form
                  case SurfaceContent.ThemeCreator(_)      => translators.form
                  case SurfaceContent.FileSearch(_)        => translators.form
                  case SurfaceContent.ContextualToolbar(_) => translators.form
                  case SurfaceContent.CommentLens(_)       => translators.form
                  case SurfaceContent.StartPage(_)         => translators.editor
                  case _                                   => translators.peekOverlay
          case None =>
            translators.editor

    if state.hasBlockingModal then translators.form
    else if recordingBinding then new HotkeyRecordingTranslator(now)
    else CompositeTranslator(translators.globalHotkey, localTranslator)
