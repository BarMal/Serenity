package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** A new buffer, and the state to fall back to if committing it is rejected. */
final private[manager] case class BufferCreation(idAdvanced: AppState, created: AppState, bufferId: BufferId)

/** A buffer's replaced content, and the LSP `didChange` it calls for once committed (`(uri, language, text)`). */
final private[manager] case class BufferContentReplacement(
    state: AppState,
    buffer: Buffer,
    documentChange: Option[(String, LanguageId, String)]
)

/** Focus switching, pane operations and buffer creation as pure functions of the state `StateManagerEditorCapability`
  * commits.
  */
private[manager] object EditorTransitions:

  def focused(state: AppState, focus: Focus): AppState =
    state.copy(persisted = state.persisted.copy(focus = focus))

  def paneSwitched(state: AppState, paneId: PaneId): Option[AppState] =
    Option.when(state.persisted.layout.editorPanes.contains(paneId))(
      state.copy(persisted =
        state.persisted.copy(
          layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId)),
          focus = Focus.EditorPane(paneId)
        )
      )
    )

  def paneInserted(
    state: AppState,
    requestedAfter: Option[PaneId],
    bufferId: Option[BufferId],
    splitAxis: SplitAxis
  ): (AppState, PaneId) =
    val paneId = state.runtime.nextPaneId
    val pane = bufferId match
      case Some(id) => EditorPane.withBuffer(paneId, id)
      case None     => EditorPane.empty(paneId)
    val targetPaneId =
      requestedAfter
        .filter(state.persisted.layout.editorPanes.contains)
        .orElse(state.persisted.layout.orderedPaneIds.lastOption)

    val updatedTree =
      targetPaneId match
        case Some(target) =>
          state.persisted.layout.workspaceTree.flatMap(
            _.split(
              target,
              paneId,
              splitAxis,
              WorkspaceNodeId(s"split-${target.value}-${paneId.value}"),
              WorkspaceNodeId(s"editor-${paneId.value}")
            )
          )
        case None =>
          Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))

    updatedTree match
      case Some(tree) =>
        val updatedState = state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = state.persisted.layout.editorPanes.updated(paneId, pane),
              activeEditorPaneId = Some(paneId),
              workspaceTree = Some(tree)
            ),
            focus = Focus.EditorPane(paneId)
          ),
          runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
        )
        (updatedState, paneId)
      case None =>
        (state, paneId)

  /** Advancing `nextBufferId` can never by itself violate an invariant (it never touches `buffers`/`bufferOrder`), so
    * the fallback already has it advanced: a drifted `nextBufferId` that collides with a live buffer is consumed even
    * when the structural add is rejected, instead of reverting straight back to the collision.
    */
  def bufferCreated(state: AppState, content: String, filePath: Option[Path])(using Balance): BufferCreation =
    val bufferId   = state.runtime.nextBufferId
    val idAdvanced = state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1)))
    val buffer =
      if content.isEmpty && filePath.isEmpty then Buffer.newEmpty(bufferId)
      else
        val fresh = Buffer.fromString(bufferId, content)
        fresh.copy(document = fresh.document.copy(filePath = filePath))
    val created = idAdvanced.copy(persisted =
      idAdvanced.persisted.copy(
        buffers = idAdvanced.persisted.buffers + (bufferId -> buffer),
        bufferOrder = idAdvanced.persisted.bufferOrder :+ bufferId
      )
    )
    BufferCreation(idAdvanced, created, bufferId)

  def bufferContentReplaced(state: AppState, bufferId: BufferId, content: String)(using
    Balance
  ): Option[BufferContentReplacement] =
    state.persisted.buffers.get(bufferId).map { buffer =>
      val updatedBuffer = buffer.copy(
        document = buffer.document.copy(content = Rope(content), isDirty = true, isNewEmpty = false)
      )
      val documentChange =
        if buffer.document.content.collect() == content then None
        else
          for
            path       <- updatedBuffer.document.filePath
            languageId <- updatedBuffer.document.language
          yield (path.toUri.toString, languageId, content)
      BufferContentReplacement(
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))),
        updatedBuffer,
        documentChange
      )
    }
