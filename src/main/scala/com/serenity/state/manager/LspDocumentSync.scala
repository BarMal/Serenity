package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.foldable.*
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.{DocumentDelta, LspEffect}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}

/** State, effect interpretation, and candidate-buffer computation the event pipeline exposes for LSP document-change
  * synchronisation. `candidateLspBufferIds` stays owned by the pipeline (and its own spec) since it is shared with
  * markdown-preview-commit scheduling, not exclusive to LSP sync. As a capability record rather than a trait -- nothing
  * here breaks a construction-order cycle (#1389), so mockability is the only reason this needs an interface at all,
  * and a record fakes trivially without one (#1017).
  */
final private[manager] case class LspDocumentSyncPort(
    currentState: IO[AppState],
    interpretEffect: AppEffect => IO[Unit],
    candidateLspBufferIds: (AppState, AppState) => Set[BufferId]
)

/** Notifies the LSP queue of buffer content changes after each event dispatch, independent of event dispatch and focus
  * routing.
  */
final private[manager] class LspDocumentSync(port: LspDocumentSyncPort):
  import port.*

  /** A document that grows past the large-file limit is closed on the server and stays out of its sight; see
    * [[LargeFilePolicy]].
    *
    * A prose workspace never announces its documents to a server (see `announceOpenedToLsp`), so its edits have no one
    * to sync to and must not pay for collecting the whole text on every keystroke (#1834).
    */
  def enqueueChangedLspDocuments(previousState: AppState): IO[Unit] =
    currentState.flatMap { currentState =>
      if !currentState.editingContext.hasCodeTooling then IO.unit
      else enqueueChanged(previousState, currentState)
    }

  private def enqueueChanged(previousState: AppState, currentState: AppState): IO[Unit] =
    candidateLspBufferIds(previousState, currentState).toList.traverse_ { bufferId =>
      currentState.persisted.buffers.get(bufferId) match
        case None => IO.unit
        case Some(buffer) =>
          val previous       = previousState.persisted.buffers.get(bufferId)
          val changedContent = previous.exists(before => buffer.document.textDiffersFrom(before.document))
          val scrolled = previous.exists(before =>
            before.viewport.topLine != buffer.viewport.topLine ||
              before.viewport.visibleLines != buffer.viewport.visibleLines
          )
          val effects = for
            path       <- buffer.document.filePath.toList
            languageId <- buffer.document.language.toList
            uri        = path.toUri.toString
            largeFiles = currentState.persisted.config.largeFiles
            served     = largeFiles.allowsLanguageTooling(buffer.document)
            wasServed  = previous.forall(before => largeFiles.allowsLanguageTooling(before.document))
            change =
              if served && wasServed then
                Option.when(changedContent)(LspDocumentSync.changedDocument(uri, languageId, buffer, previous))
              else if served then Some(LspDocumentSync.reopened(uri, languageId, buffer))
              else Option.when(wasServed)(LspQueueEffect.Enqueue(LspEffect.FileClosed(uri, languageId)))
            shown = Option.when(scrolled && served)(LspDocumentSync.visibleRange(uri, languageId, buffer, currentState))
            effect <- change.toList ++ shown.toList
          yield AppEffect.LspQueue(effect)
          effects.traverse_(interpretEffect)
    }

private[manager] object LspDocumentSync:

  private def changedDocument(
    uri: String,
    languageId: LanguageId,
    buffer: Buffer,
    previous: Option[Buffer]
  ): LspQueueEffect =
    val delta = previous.flatMap { before =>
      buffer.document
        .changeFrom(before.document)
        .map(DocumentDelta(_, before.document.contentVersion, buffer.document.contentVersion))
    }
    LspQueueEffect.DocumentChanged(uri, languageId, buffer.document.content, delta)

  /** A document that shrank back under the large-file limit is announced afresh: the server let go of it on the way up.
    */
  private def reopened(uri: String, languageId: LanguageId, buffer: Buffer): LspQueueEffect =
    LspQueueEffect.Enqueue(LspEffect.FileOpened(uri, languageId, buffer.document.content))

  private def visibleRange(uri: String, languageId: LanguageId, buffer: Buffer, state: AppState): LspQueueEffect =
    val lines = VisibleBufferLines.of(buffer, state)
    LspQueueEffect.Enqueue(LspEffect.VisibleRangeChanged(uri, languageId, lines.first, lines.last))

  def announceClosed(lspQueue: LspEffectQueue)(before: AppState, after: AppState): IO[Unit] =
    closedDocuments(before, after).traverse_(lspQueue.enqueue)

  /** The documents a commit stopped managing: a buffer left `persisted.buffers` and no open buffer holds its path any
    * more, so closing one of two tabs on the same file leaves the server's copy open for the other.
    */
  def closedDocuments(before: AppState, after: AppState): List[LspEffect] =
    val removed =
      if before.persisted.buffers eq after.persisted.buffers then Nil
      else before.persisted.buffers.values.toList.filterNot(buffer => after.persisted.buffers.contains(buffer.id))
    if removed.isEmpty then Nil
    else
      val stillOpen = after.persisted.buffers.values.flatMap(_.document.filePath).toSet
      removed.flatMap { buffer =>
        for
          path       <- buffer.document.filePath
          languageId <- buffer.document.language
          if !stillOpen.contains(path)
        yield LspEffect.FileClosed(path.toUri.toString, languageId)
      }.distinct
