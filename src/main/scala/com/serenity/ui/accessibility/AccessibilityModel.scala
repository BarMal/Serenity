package com.serenity.ui.accessibility

import com.serenity.command.CommandSurfaceItem
import com.serenity.config.InterfaceDensity
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Semantic role exposed for a canvas-rendered region. */
enum AccessibilityRole:
  case Document
  case Dialog
  case Panel
  case Button
  case TextField
  case Status

/** Stable semantic description of one canvas region. */
case class AccessibleNode(
    id: String,
    role: AccessibilityRole,
    name: String,
    value: Option[String],
    selected: Boolean,
    focused: Boolean,
    bounds: LayoutRect
)

/** A polite, deduplicated update intended for an assistive technology bridge. */
case class AccessibilityAnnouncement(message: String)

/** Complete semantic projection of the custom-painted interface. */
case class AccessibilitySnapshot(
    nodes: List[AccessibleNode],
    announcements: List[AccessibilityAnnouncement]
):
  def focused: Option[AccessibleNode] = nodes.reverse.find(_.focused)

object AccessibilitySnapshot:

  /** Project the current state through the shared scene IDs used by layout and hit testing. */
  def from(
    state: AppState,
    viewport: ViewportSize,
    previous: Option[AccessibilitySnapshot] = None
  ): AccessibilitySnapshot =
    val scene = UiSceneSnapshot.from(state, viewport)
    val nodes = scene.nodesInPaintOrder.flatMap(nodeFor(state, _)) ++ surfaceControls(state, scene)
    AccessibilitySnapshot(nodes, announcements(previous, nodes))

  /** Comfortable and spacious surfaces reserve two text rows for pointer targets; compact remains keyboard complete. */
  def minimumTargetRows(density: InterfaceDensity): Int =
    SurfaceFrameLayout.minimumTargetRows(density)

  private def nodeFor(state: AppState, node: SceneNode): Option[AccessibleNode] =
    node.id match
      case SceneNodeId.EditorPane(paneId) =>
        val buffer = state.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.buffers.get)
        val name = buffer
          .flatMap(_.filePath)
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .getOrElse("Untitled document")
        Some(
          AccessibleNode(
            s"pane:${paneId.value}",
            AccessibilityRole.Document,
            name,
            buffer.map(_.content.toString),
            false,
            state.focus == Focus.EditorPane(paneId),
            node.contentRect
          )
        )
      case SceneNodeId.EditorPaneHeader(_) => None
      case SceneNodeId.Surface(surfaceId) =>
        state.surfaceById(surfaceId).map { surface =>
          AccessibleNode(
            s"surface:${surfaceId.value}",
            surfaceRole(surface.content),
            surfaceName(surface.content),
            surfaceValue(surface.content),
            selected = false,
            focused = state.focus == Focus.Surface(surfaceId),
            node.frameRect
          )
        }

  private def surfaceControls(state: AppState, scene: UiSceneSnapshot): List[AccessibleNode] =
    scene.nodesInPaintOrder.flatMap {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) =>
          state.surfaceById(surfaceId).toList.flatMap(surface => controlsFor(surface, node.frameRect, state))
      case _ => Nil
    }

  private def controlsFor(surface: UiSurface, frameRect: LayoutRect, state: AppState): List[AccessibleNode] =
    surface.content match
      case SurfaceContent.StartPage(page) =>
        page.launchActions.zipWithIndex.map {
          case (action, index) =>
            AccessibleNode(
              s"surface:${surface.id.value}/action:${action.id}",
              AccessibilityRole.Button,
              action.label,
              action.detail,
              selected = index == page.selectedIndex,
              focused = state.focus == Focus.Surface(surface.id) && index == page.selectedIndex,
              actionBounds(frameRect, index, page.launchActions.size)
            )
        }
      case SurfaceContent.CommandPalette(runner) => commandControls(surface.id, runner, frameRect, state)
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, _) =>
        commandControls(surface.id, runner, runner.submenuItems(groupId), SurfaceContent.CommandPaletteSubmenu(runner, groupId, false), frameRect, state)
      case SurfaceContent.ContextMenu(menu) => menuControls(surface.id, menu, frameRect, state)
      case SurfaceContent.ContextualToolbar(toolbarState) => toolbarControls(surface.id, toolbarState, frameRect, state)
      case SurfaceContent.ModalWorkflow(modal) => modalControls(surface.id, modal, frameRect, state)
      case content if isPinned(surface.presentation) => pinnedControls(surface.id, content, frameRect, state)
      case _ => Nil

  private def commandControls(
    surfaceId: SurfaceId,
    runner: com.serenity.command.CommandRunner,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    commandControls(surfaceId, runner, runner.visibleItems, SurfaceContent.CommandPalette(runner), frameRect, state)

  private def commandControls(
    surfaceId: SurfaceId,
    runner: com.serenity.command.CommandRunner,
    items: List[CommandSurfaceItem],
    content: SurfaceContent,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, content)
    val targetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.config.interfaceDensity)
    val itemWindow = frame.itemWindow(
      itemCount = items.size,
      selectedIndex = runner.selectedIndex,
      hasHeader = true,
      hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
      itemGapRows = state.config.commandRunnerItemGapRows,
      itemTargetRows = targetRows
    )
    val visibleItems = itemWindow.slice(items)
    val itemBounds = frame
      .contentRowSlots(
        visibleItems.size,
        hasHeader = true,
        hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
        itemGapRows = state.config.commandRunnerItemGapRows,
        itemTargetRows = targetRows
      )
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) => index -> y }
      .toMap
    visibleItems.zipWithIndex.flatMap {
      case (item, index) =>
        val selected = runner.selectedItem.exists(_.id == item.id)
        val role = item match
          case _: CommandSurfaceItem.InputItem => AccessibilityRole.TextField
          case _                               => AccessibilityRole.Button
        itemBounds.get(index).map { y =>
          AccessibleNode(
            s"surface:${surfaceId.value}/item:${item.id}",
            role,
            itemLabel(item),
            itemValue(item),
            selected,
            focused = state.focus == Focus.Surface(surfaceId) && selected,
            LayoutRect(
              frame.contentRect.x,
              y,
              frame.contentRect.width,
              targetRows
            )
          )
        }
    }

  private def menuControls(surfaceId: SurfaceId, menu: ContextMenu, frameRect: LayoutRect, state: AppState): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.ContextMenu(menu))
    val window = frame.itemWindow(menu.items.size, menu.selectedIndex, hasHeader = true, hasFooter = menu.items.nonEmpty,
      itemGapRows = state.config.commandRunnerItemGapRows)
    val bounds = itemBounds(frame, window.rowCount, hasHeader = true, hasFooter = menu.items.nonEmpty,
      state.config.commandRunnerItemGapRows)
    window.slice(menu.items).zip(bounds).zipWithIndex.map { case ((item, bound), index) =>
      val selected = window.offset + index == menu.selectedIndex
      AccessibleNode(s"surface:${surfaceId.value}/item:${item.id}", AccessibilityRole.Button, item.label, None, selected,
        state.focus == Focus.Surface(surfaceId) && selected, bound)
    }

  private def modalControls(surfaceId: SurfaceId, modal: Modal, frameRect: LayoutRect, state: AppState): List[AccessibleNode] =
    val controls = modal match
      case Modal.Find(query, _, _) => List(("find", AccessibilityRole.TextField, "Find", Some(query), true))
      case Modal.ReplaceWorkflow(workflow) => List(
        ("find", AccessibilityRole.TextField, "Find", Some(workflow.findText), workflow.activeField == ReplaceWorkflowField.Find),
        ("replace", AccessibilityRole.TextField, "Replace", Some(workflow.replacementText), workflow.activeField == ReplaceWorkflowField.ReplaceWith),
        ("replace-next", AccessibilityRole.Button, "Replace Next", None, workflow.selectedAction == ReplaceWorkflowAction.ReplaceNext),
        ("replace-all", AccessibilityRole.Button, "Replace All", None, workflow.selectedAction == ReplaceWorkflowAction.ReplaceAll),
        ("current-buffer", AccessibilityRole.Button, "Current Buffer", None, workflow.selectedScope == ReplaceWorkflowScope.CurrentBuffer),
        ("selection", AccessibilityRole.Button, "Selection", None, workflow.selectedScope == ReplaceWorkflowScope.Selection)
      )
      case _ => Nil
    val frame = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.ModalWorkflow(modal))
    controls.zip(itemBounds(frame, controls.size, hasHeader = true, hasFooter = false, 0.0)).map { case ((id, role, name, value, selected), bound) =>
      AccessibleNode(s"surface:${surfaceId.value}/control:$id", role, name, value, selected,
        state.focus == Focus.Surface(surfaceId) && selected, bound)
    }

  private def toolbarControls(
    surfaceId: SurfaceId,
    toolbarState: ContextualToolbarState,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.ContextualToolbar(toolbarState))
    val items = ContextualToolbar.itemsFor(state)
    val normalized = toolbarState.normalized(items)
    val rows = ContextualToolbar.rowGroups(items, frame.contentRect.width.max(1), normalized.displayMode)
    rows.zipWithIndex.flatMap { case (row, rowIndex) =>
      val widths = ContextualToolbar.itemCellWidths(row, frame.contentRect.width.max(1), normalized.displayMode)
      val start = frame.contentRect.x + ContextualToolbar.rowLeadingPadding(row, frame.contentRect.width.max(1), normalized.displayMode)
      val positions = widths.scanLeft(start)(_ + _ + 1).dropRight(1)
      row.zip(widths).zip(positions).zipWithIndex.map { case (((item, width), x), index) =>
        val absoluteIndex = rows.take(rowIndex).map(_.size).sum + index
        val (role, value) = item match
          case ContextualToolbarItem.Input(_, _, _, input) => AccessibilityRole.TextField -> Some(input.currentValue)
          case ContextualToolbarItem.Dropdown(_, _, _, option) => AccessibilityRole.Button -> Some(option.selectedOption)
          case _ => AccessibilityRole.Button -> None
        AccessibleNode(s"surface:${surfaceId.value}/item:${item.id}", role, item.label, value,
          absoluteIndex == normalized.focusedIndex,
          state.focus == Focus.Surface(surfaceId) && absoluteIndex == normalized.focusedIndex,
          LayoutRect(x, frame.contentRect.y + rowIndex, width, 1))
      }
    }

  private def pinnedControls(surfaceId: SurfaceId, content: SurfaceContent, frameRect: LayoutRect, state: AppState): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, content)
    val resolved = SurfaceContentResolver.resolve(content, frameRect, SurfaceRenderMode.Pinned)
    resolved.rows.zip(itemBounds(frame, resolved.rows.size, resolved.header.nonEmpty, resolved.footer.nonEmpty, 0.0)).zipWithIndex.map {
      case ((row, bound), index) =>
        AccessibleNode(s"surface:${surfaceId.value}/item:$index", AccessibilityRole.Button, row.plainText, None, row.selected,
          state.focus == Focus.Surface(surfaceId) && row.selected, bound)
    }

  private def itemBounds(frame: SurfaceFrameLayout, itemCount: Int, hasHeader: Boolean, hasFooter: Boolean, itemGapRows: Double): List[LayoutRect] =
    frame.contentRowSlots(itemCount, hasHeader, hasFooter, itemGapRows).collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) => LayoutRect(frame.contentRect.x, y, frame.contentRect.width, 1)
    }

  private def isPinned(presentation: SurfacePresentation): Boolean =
    presentation match
      case SurfacePresentation.Pinned(_, _) | SurfacePresentation.Expanded(_, _) => true
      case _ => false

  private def itemLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)                             => command.label
      case CommandSurfaceItem.OptionItem(_, label, _, _, _, _)                 => label
      case CommandSurfaceItem.InputItem(_, label, _, _, _, _, _, _, _)         => label
      case CommandSurfaceItem.SettingSearchItem(_, _, _, label, _, _, _, _, _) => label
      case CommandSurfaceItem.GroupItem(_, label, _, _, _)                     => label

  private def itemValue(item: CommandSurfaceItem): Option[String] =
    item match
      case option: CommandSurfaceItem.OptionItem                               => Some(option.selectedOption)
      case CommandSurfaceItem.InputItem(_, _, _, currentValue, _, _, _, _, _)  => Some(currentValue)
      case CommandSurfaceItem.SettingSearchItem(_, _, _, _, _, value, _, _, _) => value
      case _                                                                   => None

  private def actionBounds(bounds: LayoutRect, index: Int, count: Int): LayoutRect =
    val rows   = count.max(1).min(bounds.height.max(1))
    val height = math.max(1, bounds.height / rows)
    val row    = index.min(rows - 1)
    LayoutRect(
      bounds.x,
      bounds.y + row * height,
      bounds.width,
      height.min(bounds.bottom - (bounds.y + row * height))
    )

  private def surfaceRole(content: SurfaceContent): AccessibilityRole =
    content match
      case _: SurfaceContent.CommandPalette | _: SurfaceContent.CommandPaletteSubmenu |
          _: SurfaceContent.ModalWorkflow =>
        AccessibilityRole.Dialog
      case _: SurfaceContent.CursorInfoBar => AccessibilityRole.Status
      case _                               => AccessibilityRole.Panel

  private def surfaceName(content: SurfaceContent): String =
    content match
      case SurfaceContent.StartPage(page)              => page.title
      case _: SurfaceContent.CommandPalette            => "Command runner"
      case _: SurfaceContent.CommandPaletteSubmenu     => "Command runner submenu"
      case SurfaceContent.ModalWorkflow(modal)         => modal.toString
      case SurfaceContent.CursorInfoBar(_)             => "Document status"
      case SurfaceContent.MarkdownPreview(_, title)    => s"Preview: $title"
      case SurfaceContent.QuickInfo(_)                 => "Quick information"
      case SurfaceContent.FilePreview(path, _)         => s"Preview: ${path.getFileName}"
      case SurfaceContent.SymbolDefinition(symbol, _)  => s"Symbol: $symbol"
      case SurfaceContent.DirectoryListing(path, _, _) => s"Directory: ${path.getFileName}"
      case SurfaceContent.DirectoryTree(_, _)          => "Directory tree"
      case SurfaceContent.ThemePicker(_)               => "Theme picker"
      case SurfaceContent.ThemeCreator(_)              => "Theme creator"
      case SurfaceContent.FileSearch(_)                => "File search"
      case SurfaceContent.ContextualToolbar(_)         => "Contextual toolbar"
      case SurfaceContent.ContextMenu(menu)            => menu.title
      case SurfaceContent.CommentLens(_)               => "Comment"
      case SurfaceContent.Terminal(_, _)               => "Terminal"
      case SurfaceContent.Outline(_, _)                => "Outline"
      case SurfaceContent.Diagnostics(_, _)            => "Diagnostics"
      case SurfaceContent.GhostOverlay(original, _)    => surfaceName(original)

  private def surfaceValue(content: SurfaceContent): Option[String] =
    content match
      case SurfaceContent.StartPage(page)     => page.statusMessage
      case SurfaceContent.CursorInfoBar(text) => Some(text)
      case _                                  => None

  private def announcements(
    previous: Option[AccessibilitySnapshot],
    nodes: List[AccessibleNode]
  ): List[AccessibilityAnnouncement] =
    val prior = previous.map(_.nodes.map(node => node.id -> node).toMap).getOrElse(Map.empty)
    nodes.flatMap { node =>
      val focus =
        Option.when(node.focused && !prior.get(node.id).exists(_.focused))(AccessibilityAnnouncement(node.name))
      val status = Option
        .when(
          node.role == AccessibilityRole.Status && !prior
            .get(node.id)
            .flatMap(_.value)
            .contains(node.value.getOrElse(""))
        )(node.value.map(AccessibilityAnnouncement.apply))
        .flatten
      List(focus, status).flatten
    }
