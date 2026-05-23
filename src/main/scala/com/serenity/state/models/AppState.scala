package com.serenity.state.models

import com.serenity.animation.AnimationState
import com.serenity.command.CommandRunner
import com.serenity.config.AppConfig
import com.serenity.ui.layout.{Layout, PeekOverlay, TerminalSize}
import com.serenity.ui.theme.Theme

case class FindState(
    query: String,
    resultLines: List[Int],
    currentIndex: Int
)

case class AppState(
    layout: Layout,
    buffers: Map[BufferId, Buffer],
    bufferOrder: List[BufferId] = List.empty, // Tracks buffer creation and navigation order
    focus: Focus,
    peekOverlay: Option[PeekOverlay] = None,
    modal: Option[Modal] = None,
    findState: Option[FindState] = None,
    terminalSize: Option[TerminalSize] = None,
    theme: Theme = Theme.default,
    config: AppConfig = AppConfig.default,
    commandRunner: CommandRunner = CommandRunner.empty,
    nextBufferId: BufferId = BufferId(0),
    nextPaneId: PaneId = PaneId(0),
    screenAnimations: AnimationState = AnimationState.empty
):
  /** Convenience accessor for syntax highlighting setting */
  def syntaxHighlightingEnabled: Boolean = config.syntaxHighlightingEnabled
  def isValid: Boolean                   = validationErrors.isEmpty

  /** Get the currently focused buffer ID, if any */
  def focusedBufferId: Option[BufferId] =
    focus match
      case Focus.EditorPane(paneId) =>
        layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _ => None

  /** Get the next buffer ID in navigation order */
  def nextBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if bufferOrder.isEmpty then None
    else
      val currentIndex = bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then bufferOrder.headOption
      else
        val nextIndex = (currentIndex + 1) % bufferOrder.size
        Some(bufferOrder(nextIndex))

  /** Get the previous buffer ID in navigation order */
  def previousBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if bufferOrder.isEmpty then None
    else
      val currentIndex = bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then bufferOrder.headOption
      else
        val prevIndex = (currentIndex - 1 + bufferOrder.size) % bufferOrder.size
        Some(bufferOrder(prevIndex))

  def validationErrors: List[String] =
    val errors = List.newBuilder[String]

    // Focus validation
    focus match
      case Focus.EditorPane(paneId) if !layout.editorPanes.contains(paneId) =>
        errors += s"Focus points to non-existent pane: $paneId"
      case Focus.PinnedPanel(pos) if !layout.pinnedPanels.contains(pos) =>
        errors += s"Focus points to non-existent panel: $pos"
      case Focus.PeekOverlay if peekOverlay.isEmpty =>
        errors += "Focus on PeekOverlay but no overlay exists"
      case Focus.Modal(_) if modal.isEmpty =>
        errors += "Focus on Modal but no modal exists"
      case Focus.CommandRunner if !commandRunner.isActive =>
        errors += "Focus on CommandRunner but runner is not active"
      case _ => // Valid focus
    // Buffer-Pane consistency
    layout.editorPanes.foreach { (paneId, pane) =>
      pane.bufferId.foreach { bufferId =>
        if !buffers.contains(bufferId) then errors += s"Pane $paneId references non-existent buffer: $bufferId"
      }
    }

    errors.result()

  def validated: Either[List[String], AppState] =
    if isValid then Right(this) else Left(validationErrors)

object AppState:

  def initial(using com.serenity.rope.Balance): AppState =
    val initialBufferId = BufferId(0)
    val initialBuffer   = Buffer.newEmpty(initialBufferId)
    val initialPane     = EditorPane.withBuffer(PaneId(0), initialBufferId)
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0))
    )
    AppState(
      layout = layout,
      buffers = Map(initialBufferId -> initialBuffer),
      bufferOrder = List(initialBufferId),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(1),
      nextPaneId = PaneId(1)
    )

  def empty: AppState =
    AppState(
      layout = Layout.empty,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0))
    )
