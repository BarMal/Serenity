package com.serenity.app

import cats.effect.IO
import cats.effect.std.Dispatcher
import com.serenity.state.manager.StateUpdater
import com.serenity.state.models.AppState
import org.typelevel.log4cats.Logger

private[serenity] object AppRuntimeCallbacks:

  /** Bridges the TUI's spawned Markdown preview window (issue #1113) closing via its own native close control back into
    * application state: the window only hides itself (see `MarkdownPreviewWindow.resource`), so this callback's sole
    * job is toggling `markdownPreviewWindowBuffer` back off rather than orphaning a dead window reference.
    */
  private[serenity] def markdownPreviewCloseCallbackBridge(
    stateManager: StateUpdater,
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatcher.unsafeRunAndForget(
        stateManager
          .updateStateValidated(closeMarkdownPreviewWindowInState)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] markdown preview close callback failed"))
      )

  private[serenity] def closeMarkdownPreviewWindowInState(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = None))
