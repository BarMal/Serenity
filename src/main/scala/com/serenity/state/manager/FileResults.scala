package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutEngine

private[manager] enum SaveKind:
  case Save

  /** The "Overwrite" answer to a reload conflict: no revision check. */
  case Force
  case SaveAs

/** A save as submitted (#1671). `snapshot` is the buffer at submit time: its content is the version the save writes,
  * and its path is the one the buffer must still have for the result to apply.
  */
final private[manager] case class FileSave(
    bufferId: BufferId,
    target: Path,
    snapshot: Buffer,
    kind: SaveKind,
    writtenAtSubmit: Long
)

/** How each file result merges into the state current when it arrives (#1671, #1672). */
private[manager] object FileResults:

  /** Records the save's metadata on the buffer; it is only marked clean, and only takes the saved rich-text document,
    * if its content is still the content that was written. Rope equality short-circuits on identity, so the common
    * "nothing typed since" case costs nothing, and an undo back to the saved text still counts as unchanged.
    */
  def saved(state: AppState, save: FileSave, saved: Buffer): AppState =
    val merged =
      state.persisted.buffers
        .get(save.bufferId)
        .filter(_.document.filePath == save.snapshot.document.filePath)
        .fold(state) { current =>
          val unchanged = current.document.content == save.snapshot.document.content
          val document = current.document.copy(
            filePath = saved.document.filePath,
            language = saved.document.language,
            revision = saved.document.revision,
            isDirty = current.document.isDirty && !unchanged
          )
          val richText =
            if unchanged then
              current.richText.copy(richTextDocument = saved.richText.richTextDocument, richTextFidelity = None)
            else current.richText.copy(richTextFidelity = None)
          withBuffer(state, current.copy(document = document, richText = richText))
        }
    if save.kind == SaveKind.SaveAs then withRecentFile(merged, save.target) else merged

  /** Replaces the buffer's content with the disk's, unless it was edited after the reload was requested. */
  def reloaded(
    state: AppState,
    bufferId: BufferId,
    path: Path,
    contentAtRequest: com.serenity.rope.Rope,
    disk: Buffer
  ): AppState =
    state.persisted.buffers
      .get(bufferId)
      .filter(buffer => buffer.document.filePath.contains(path) && buffer.document.content == contentAtRequest)
      .fold(state)(current =>
        withBuffer(state, current.copy(document = disk.document, richText = disk.richText).clampedToContent)
      )

  /** Adds the loaded buffer under a fresh id, focuses it and dismisses the start page -- only the start page: every
    * other surface, docked panels included, is part of the workspace the file opens into (#1672).
    */
  def loaded(state: AppState, path: Path, loaded: Buffer): AppState =
    // Skipping ids in use keeps a drifted `nextBufferId` (#858) from overwriting a live buffer.
    val bufferId =
      Iterator
        .iterate(state.runtime.nextBufferId)(id => BufferId(id.value + 1))
        .dropWhile(state.persisted.buffers.contains)
        .next()
    val withLoaded = state.copy(
      persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> loaded.copy(id = bufferId))),
      runtime = state.runtime.copy(
        nextBufferId = BufferId(bufferId.value + 1),
        uiSurfaces = state.runtime.uiSurfaces.filterNot(_.content match
          case SurfaceContent.StartPage(_) => true
          case _                           => false)
      )
    )
    val focused = EditorState.focusBuffer(
      EditorState.rebalancePanes(EditorState.insertBufferInOrder(withLoaded, bufferId), Some(bufferId)),
      bufferId
    )
    val resized =
      focused.runtime.viewportSize.fold(focused)(viewportSize =>
        LayoutEngine.syncViewportDimensions(focused, viewportSize)
      )
    withRecentFile(resized, path)

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, buffer)))

  private def withRecentFile(state: AppState, path: Path): AppState =
    state.copy(persisted =
      state.persisted.copy(
        recentFiles = (path :: state.persisted.recentFiles.filterNot(_ == path)).take(20),
        recentFilesByMode =
          Persisted.trackRecentFile(state.persisted.recentFilesByMode, state.persisted.config.appMode, path)
      )
    )
