package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEditSupport, NoticeReducer}
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
          // A save in a format that cannot hold blocks turns them into empty lines, so the saved text differs.
          val converted = unchanged && saved.document.content != current.document.content
          val base      = if converted then current.document.withContent(saved.document.content) else current.document
          val document = base.copy(
            filePath = saved.document.filePath,
            language = saved.document.language,
            revision = saved.document.revision,
            encoding = saved.document.encoding,
            hasBom = saved.document.hasBom,
            mixedLineEndings = None,
            mixedNoticePending = false,
            isDirty = current.document.isDirty && !unchanged,
            savedGeneration = current.document.savedGeneration + 1
          )
          val richText =
            if unchanged then
              // `unchanged` just proved `saved`'s document matches `current`'s content exactly, so it's synced at
              // `current`'s *current* content version -- which may have moved since `save.snapshot` was taken (e.g.
              // an edit undone back to the saved text bumps `contentVersion` without changing the text) (#1663).
              current.richText
                .withSyncedDocument(saved.richText.richTextDocument, document.contentVersion)
                .copy(richTextFidelity = None)
            else current.richText.copy(richTextFidelity = None)
          val updated = current.copy(document = document, richText = richText)
          withBuffer(state, if converted then updated.clampedToContent else updated)
        }
    val told =
      save.snapshot.document.mixedLineEndings
        .filter(_ => merged ne state)
        .fold(merged)(counts =>
          LineEndingChoice.withSavedMixedNotice(merged, save.bufferId, save.snapshot.document.lineEnding, counts)
        )
    val settled = NoticeReducer.withoutTopic(told, NoticeTopic.FileSave(save.bufferId))
    if save.kind == SaveKind.SaveAs then withRecentFile(settled, save.target) else settled

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
        val annotations =
          EditorEditSupport.adjustAnnotationsAcrossReplacement(
            current.annotations,
            current.document.content,
            disk.document.content
          )
        val document =
          disk.document
            .replacingContentOf(current.document)
            .keepingChosenIndentationOf(current.document)
            .keepingOpeningsOf(current.document)
            .copy(savedGeneration = current.document.savedGeneration + 1)
        withBuffer(
          state,
          current
            .copy(
              document = document,
              richText = disk.richText.withSyncedDocument(disk.richText.richTextDocument, document.contentVersion),
              annotations = annotations
            )
            .clampedToContent
        )
      )

  /** Adds the loaded buffer under a fresh id, focuses it and dismisses the start page -- only the start page: every
    * other surface, docked panels included, is part of the workspace the file opens into (#1672).
    */
  def loaded(state: AppState, path: Path, loaded: Buffer, intent: OpenIntent = OpenIntent.Tab): AppState =
    if intent != OpenIntent.Tab && openBufferFor(state, path).isDefined then surfaced(state, path, intent)
    else opened(state, path, loaded, intent)

  def openBufferFor(state: AppState, path: Path): Option[BufferId] =
    val wanted = path.normalize
    state.persisted.bufferOrder.find(id =>
      state.persisted.buffers.get(id).exists(_.document.filePath.exists(_.normalize == wanted))
    )

  /** Shows the tab of a file that is already open. Opening it to keep makes a preview tab permanent; previewing a file
    * leaves every tab as it was.
    */
  def surfaced(state: AppState, path: Path, intent: OpenIntent): AppState =
    openBufferFor(state, path).fold(state) { bufferId =>
      val shown = EditorState.focusBuffer(EditorState.rebalancePanes(state, Some(bufferId)), bufferId)
      if intent == OpenIntent.Keep && shown.runtime.previewTab.exists(_.bufferId == bufferId) then
        shown.copy(runtime = shown.runtime.copy(previewTab = None))
      else shown
    }

  /** A preview tab that is still one: untouched since it opened, so nothing in it can be lost by replacing it. */
  private def replaceablePreview(state: AppState): Option[BufferId] =
    state.runtime.previewTab.collect {
      case PreviewTab(bufferId, version)
          if state.persisted.buffers
            .get(bufferId)
            .exists(b => !b.document.isDirty && b.document.contentVersion == version) =>
        bufferId
    }

  private def asPreview(state: AppState, bufferId: BufferId): AppState =
    val replaced = replaceablePreview(state).filter(_ != bufferId)
    val version  = state.persisted.buffers.get(bufferId).fold(0L)(_.document.contentVersion)
    val tab      = Some(PreviewTab(bufferId, version))
    replaced.fold(state.copy(runtime = state.runtime.copy(previewTab = tab))) { old =>
      val inPlace = state.persisted.bufferOrder.filterNot(_ == bufferId).map(id => if id == old then bufferId else id)
      val removed = EditorState.removeBuffer(state, old)
      removed.copy(
        persisted = removed.persisted.copy(bufferOrder = inPlace),
        runtime = removed.runtime.copy(previewTab = tab)
      )
    }

  private def opened(state: AppState, path: Path, loaded: Buffer, intent: OpenIntent): AppState =
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
    val placed = if intent == OpenIntent.Preview then asPreview(resized, bufferId) else resized
    withRecentFile(placed, path)

  def withoutRecentFiles(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(recentFiles = Nil, recentFilesByMode = Map.empty, recentFolders = Nil))

  def withRecentFolder(state: AppState, folder: Path): AppState =
    state.copy(persisted =
      state.persisted.copy(recentFolders = Persisted.trackRecentFolder(state.persisted.recentFolders, folder))
    )

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, buffer)))

  private def withRecentFile(state: AppState, path: Path): AppState =
    state.copy(persisted =
      state.persisted.copy(
        recentFiles = (path :: state.persisted.recentFiles.filterNot(_ == path)).take(Persisted.RecentLimit),
        recentFilesByMode =
          Persisted.trackRecentFile(state.persisted.recentFilesByMode, state.persisted.config.appMode, path)
      )
    )
