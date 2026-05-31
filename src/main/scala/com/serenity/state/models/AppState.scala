package com.serenity.state.models

import com.serenity.animation.AnimationState
import com.serenity.config.AppConfig
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.theme.Theme

enum SurfacePhase:
  case BufferFadingOut  // surface not rendered; buffer chars in overlay area fading out
  case Visible          // surface rendered (may have fade-in animation)
  case Exiting          // ghost surface fading out; focus already restored

case class SurfaceAnimationState(
    phase: SurfacePhase = SurfacePhase.Visible,
    animationState: AnimationState = AnimationState.empty,
    overlayHeight: Int = 0,     // rows in overlay (excluding border) for building fade-in
    bufferFadeLength: Int = 0,  // ticks to stay in BufferFadingOut before transitioning
    phaseTick: Int = 0          // ticks elapsed in current phase
)

case class FindState(
    query: String,
    resultLines: List[Int],
    currentIndex: Int
)

case class ThemeTransition(previousTheme: Theme, currentStep: Int, totalSteps: Int):
  def progress: Double        = if totalSteps <= 0 then 1.0 else currentStep.toDouble / totalSteps
  def advance: ThemeTransition = copy(currentStep = currentStep + 1)
  def isComplete: Boolean     = currentStep >= totalSteps

case class AppState(
    layout: Layout,
    buffers: Map[BufferId, Buffer],
    bufferOrder: List[BufferId] = List.empty, // Tracks buffer creation and navigation order
    focus: Focus,
    uiSurfaces: List[UiSurface] = List.empty,
    actionStack: List[AppAction] = Nil,
    findState: Option[FindState] = None,
    viewportSize: Option[ViewportSize] = None,
    theme: Theme = Theme.default,
    config: AppConfig = AppConfig.default,
    nextBufferId: BufferId = BufferId(0),
    nextPaneId: PaneId = PaneId(0),
    nextSurfaceId: Int = 0,
    themeTransition: Option[ThemeTransition] = None,
    surfaceAnimations: Map[SurfaceId, SurfaceAnimationState] = Map.empty,
    clipboard: Option[String] = None // not persisted between sessions
):
  /** Convenience accessor for syntax highlighting setting */
  def syntaxHighlightingEnabled: Boolean = config.syntaxHighlightingEnabled
  def isValid: Boolean                   = validationErrors.isEmpty

  /** Cursor position for the currently active editor pane, if any. */
  def activeCursorPosition: Option[CursorPosition] =
    layout.activeEditorPaneId
      .flatMap(layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(buffers.get)
      .flatMap(_.cursors.headOption)

  def floatingSurfaces: List[UiSurface] =
    uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  def pinnedSurfaces: List[UiSurface] =
    uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => true
        case _                                => false
    }

  def surfaceById(surfaceId: SurfaceId): Option[UiSurface] =
    uiSurfaces.find(_.id == surfaceId)

  def activeSurface: Option[UiSurface] =
    focus match
      case Focus.Surface(surfaceId) => surfaceById(surfaceId)
      case _                        => None

  def commandRunnerSurface: Option[UiSurface] =
    uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.CommandPalette])

  def themePickerSurface: Option[UiSurface] =
    uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.ThemePicker])

  def fileSearchSurface: Option[UiSurface] =
    uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.FileSearch])

  def startPageSurface: Option[UiSurface] =
    uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.StartPage])

  def modalSurface: Option[UiSurface] =
    uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.ModalWorkflow])

  def peekSurface: Option[UiSurface] =
    uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }

  def allocateSurfaceId: (AppState, SurfaceId) =
    val surfaceId = SurfaceId(s"surface-$nextSurfaceId")
    (copy(nextSurfaceId = nextSurfaceId + 1), surfaceId)

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
      case Focus.Surface(surfaceId) if !uiSurfaces.exists(_.id == surfaceId) =>
        errors += s"Focus points to non-existent surface: $surfaceId"
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
      nextPaneId = PaneId(1),
      nextSurfaceId = 0
    )

  def empty: AppState =
    AppState(
      layout = Layout.empty,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0))
    )

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
