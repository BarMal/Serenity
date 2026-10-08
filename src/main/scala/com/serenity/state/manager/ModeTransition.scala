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

  /** `next` with what only code tooling produces dropped, when the commit leaves code mode: the server diagnostics,
    * semantic tokens and progress, and the project task record. The very `next` comes back otherwise, which is every
    * commit but a mode change. Spell-check marks stay: they belong to prose.
    */
  def settled(previous: AppState, next: Model): Model =
    if leftCodeTooling(previous, next.app) then next.copy(app = withoutCodeTooling(next.app)) else next

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
