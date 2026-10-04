package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.foldable.*
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

  /** A prose workspace never announces its documents to a server (see `announceOpenedToLsp`), so its edits have no one
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
          val changedContent =
            previousState.persisted.buffers.get(bufferId).exists(_.document.content != buffer.document.content)
          (for
            path       <- buffer.document.filePath
            languageId <- buffer.document.language
            if changedContent
          yield AppEffect.LspQueue(
            LspQueueEffect.DocumentChanged(path.toUri.toString, languageId, buffer.document.content.collect())
          ))
            .fold(IO.unit)(interpretEffect)
    }
