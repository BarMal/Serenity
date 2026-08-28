package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}

/** State, effect interpretation, and candidate-buffer computation the event pipeline exposes for LSP document-change
  * synchronisation. `candidateLspBufferIds` stays owned by the pipeline (and its own spec) since it is shared with
  * markdown-preview-commit scheduling, not exclusive to LSP sync.
  */
private[manager] trait LspDocumentSyncPort:
  def stateRef: Ref[IO, AppState]
  def interpretEffect(effect: AppEffect): IO[Unit]
  def candidateLspBufferIds(previousState: AppState, currentState: AppState): Set[BufferId]

/** Notifies the LSP queue of buffer content changes after each event dispatch, independent of event dispatch and focus
  * routing.
  */
final private[manager] class LspDocumentSync(port: LspDocumentSyncPort):
  import port.*

  def enqueueChangedLspDocuments(previousState: AppState): IO[Unit] =
    stateRef.get.flatMap { currentState =>
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
    }
