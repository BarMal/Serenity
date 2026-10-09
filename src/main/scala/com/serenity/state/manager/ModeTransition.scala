package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.foldable.*
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*

/** What leaving or entering code mode does, in one place so that every way the app mode can change -- the command, a
  * workflow preset, the config file edited from outside -- runs the same lifecycle: [[settled]] inside the commit,
  * [[lspEffects]] and the project task's stopping after it.
  *
  * A prose workspace has no code tooling (`EditingContext.hasCodeTooling`), so leaving code mode lets the language
  * servers go and stops the project task, and entering it opens the documents already open to the servers again.
  */
private[manager] object ModeTransition:

  def changed(before: AppState, after: AppState): Boolean =
    before.persisted.config.appMode != after.persisted.config.appMode

  def leftCodeTooling(before: AppState, after: AppState): Boolean =
    changed(before, after) && before.editingContext.hasCodeTooling && !after.editingContext.hasCodeTooling

  def enteredCodeTooling(before: AppState, after: AppState): Boolean =
    changed(before, after) && !before.editingContext.hasCodeTooling && after.editingContext.hasCodeTooling

  /** `next` with what the mode it leaves behind owned dropped, in the one write that changes the mode: the docked
    * panels the new mode does not offer and, on leaving code mode, what only code tooling produces -- the server
    * diagnostics, semantic tokens and progress, and the project task record. The very `next` comes back when nothing is
    * left to drop, which is every commit but a mode change. Spell-check marks stay: they belong to prose.
    *
    * The hidden panels are not recorded as an undo step: undoing one would dock a code panel in a prose workspace.
    */
  def settled(previous: AppState, next: Model): Model =
    val panelsOutside = if changed(previous, next.app) then panelsOutsideMode(next.app) else Nil
    if panelsOutside.isEmpty && !leftCodeTooling(previous, next.app) then next
    else
      val withoutPanels = panelsOutside.foldLeft(next.app)((state, id) => PanelTransitions.removePanel(id)(state))
      next.copy(app = if leftCodeTooling(previous, next.app) then withoutCodeTooling(withoutPanels) else withoutPanels)

  /** The documents a server is told about, or told to forget, for a mode change: those with a language and a file, one
    * per file, in buffer order.
    */
  def lspEffects(before: AppState, after: AppState): List[LspEffect] =
    if leftCodeTooling(before, after) then released(documents(before))
    else if enteredCodeTooling(before, after) then
      documents(after).map((uri, language, text) => LspEffect.FileOpened(uri, language, text))
    else Nil

  /** Nothing was ever announced to a server without a document to announce, so with none there is nothing to release.
    */
  private def released(open: List[(String, LanguageId, Rope)]): List[LspEffect] =
    if open.isEmpty then Nil
    else open.map((uri, language, _) => LspEffect.FileClosed(uri, language)) :+ LspEffect.ReleaseAll

  def announceLsp(lspQueue: LspEffectQueue)(before: AppState, after: AppState): IO[Unit] =
    lspEffects(before, after).traverse_(lspQueue.enqueue)

  /** The project task a mode change leaves without a home, to be stopped after the commit that released its record. */
  def stoppedTask(before: AppState, after: AppState): Option[RunningProjectTask] =
    if leftCodeTooling(before, after) then before.runtime.projectTasks.running else None

  def taskStoppedNotice(task: RunningProjectTask): Notice =
    Notice(
      NoticeLevel.Info,
      s"Stopped the ${task.command.kind.lowerLabel} task: project tasks are only available in code mode."
    )

  private def documents(state: AppState): List[(String, LanguageId, Rope)] =
    state.persisted.buffers.values.toList
      .sortBy(_.id.value)
      .flatMap { buffer =>
        for
          path     <- buffer.document.filePath
          language <- buffer.document.language
        yield (path.toUri.toString, language, buffer.document.content)
      }
      .distinctBy((uri, _, _) => uri)

  private def panelsOutsideMode(state: AppState): List[PanelId] =
    val mode = state.persisted.config.appMode
    PanelId.values.toList.filter { id =>
      !PanelRegistry.registrationFor(id).family.modes.contains(mode) &&
      state.runtime.uiSurfaces.exists(surface => PanelId.forContent(surface.content).contains(id))
    }

  private def withoutCodeTooling(state: AppState): AppState =
    ProjectTaskTransitions.stoppedOnLeavingCode(withoutServerData(state))

  private def withoutServerData(state: AppState): AppState =
    val languageService = state.runtime.languageService
    val diagnostics     = languageService.diagnosticsState
    state.copy(runtime =
      state.runtime.copy(languageService =
        languageService.copy(
          diagnosticsState = diagnostics.copy(diagnostics = SpellChecker.onlySpellCheck(diagnostics.diagnostics)),
          semanticTokensState = SemanticTokensState(),
          progress = Map.empty
        )
      )
    )
