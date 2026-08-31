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
final case class AccessibleNode(
    id: String,
    role: AccessibilityRole,
    name: String,
    value: Option[String],
    selected: Boolean,
    focused: Boolean,
    bounds: LayoutRect
)

/** A polite, deduplicated update intended for an assistive technology bridge. */
final case class AccessibilityAnnouncement(message: String)

/** Complete semantic projection of the custom-painted interface. */
final case class AccessibilitySnapshot(
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
    val visibleNodes = state.topModalSurface match
      case Some(surface) => scene.modal.filter(_.id == SceneNodeId.Surface(surface.id))
      case None          => scene.nodesInPaintOrder
    val nodes = visibleNodes.flatMap(nodeFor(state, _)) ++ surfaceControls(state, visibleNodes)
    AccessibilitySnapshot(nodes, announcements(previous, nodes))

  /** Comfortable and spacious surfaces reserve two text rows for pointer targets; compact remains keyboard complete. */
  def minimumTargetRows(density: InterfaceDensity): Int =
    SurfaceFrameLayout.minimumTargetRows(density)

  private def nodeFor(state: AppState, node: SceneNode): Option[AccessibleNode] =
    node.id match
      case SceneNodeId.EditorPane(paneId) =>
        val buffer =
          state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
        val name = buffer
          .flatMap(_.document.filePath)
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .getOrElse("Untitled document")
        Some(
          AccessibleNode(
            s"pane:${paneId.value}",
            AccessibilityRole.Document,
            name,
            buffer.map(_.document.content.toString),
            false,
            state.persisted.focus == Focus.EditorPane(paneId),
            node.contentRect
          )
        )
      case SceneNodeId.EditorPaneHeader(_) => None
      case SceneNodeId.ModalBackdrop       => None
      case SceneNodeId.Surface(surfaceId) =>
        state.surfaceById(surfaceId).map { surface =>
          AccessibleNode(
            s"surface:${surfaceId.value}",
            surfaceRole(surface.content),
            surfaceName(surface.content),
            surfaceValue(surface.content),
            selected = false,
            focused = state.persisted.focus == Focus.Surface(surfaceId),
            node.frameRect
          )
        }

  private def surfaceControls(state: AppState, nodes: List[SceneNode]): List[AccessibleNode] =
    nodes.flatMap {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) =>
        state.surfaceById(surfaceId).toList.flatMap { surface =>
          controlsFor(surface, node.frameRect, state) ++ statusFor(surface, node.frameRect)
        }
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
              focused = state.persisted.focus == Focus.Surface(surface.id) && index == page.selectedIndex,
              actionBounds(frameRect, index, page.launchActions.size)
            )
        }
      case SurfaceContent.CommandPalette(runner)          => commandControls(surface.id, runner, frameRect, state)
      case SurfaceContent.ContextMenu(menu)               => menuControls(surface.id, menu, frameRect, state)
      case SurfaceContent.ContextualToolbar(toolbarState) => toolbarControls(surface.id, toolbarState, frameRect, state)
      case SurfaceContent.ModalWorkflow(modal)            => modalControls(surface.id, modal, frameRect, state)
      case content if isPinned(surface.presentation)      => pinnedControls(surface.id, content, frameRect, state)
      case _                                              => Nil

  private def statusFor(surface: UiSurface, frameRect: LayoutRect): List[AccessibleNode] =
    statusMessage(surface.content).toList.map { message =>
      AccessibleNode(
        s"surface:${surface.id.value}/status",
        AccessibilityRole.Status,
        "Status",
        Some(message),
        selected = false,
        focused = false,
        LayoutRect(frameRect.x, frameRect.bottom - 1, frameRect.width, 1)
      )
    }

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
    val frame      = SurfaceFrameLayout.forContent(frameRect, content)
    val targetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
    val itemWindow = frame.itemWindow(
      itemCount = items.size,
      selectedIndex = runner.selectedIndex,
      hasHeader = true,
      hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
      itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
      itemTargetRows = targetRows
    )
    val visibleItems = itemWindow.slice(items)
    val itemBounds = frame
      .contentRowSlots(
        visibleItems.size,
        hasHeader = true,
        hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
        itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
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
            focused = state.persisted.focus == Focus.Surface(surfaceId) && selected,
            LayoutRect(
              frame.contentRect.x,
              y,
              frame.contentRect.width,
              targetRows
            )
          )
        }
    }

  private def menuControls(
    surfaceId: SurfaceId,
    menu: ContextMenu,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.ContextMenu(menu))
    val targetRows =
      SurfaceFrameLayout.itemTargetRowsFor(SurfaceContent.ContextMenu(menu), state.persisted.config.interfaceDensity)
    val window = frame.itemWindow(
      menu.items.size,
      menu.selectedIndex,
      hasHeader = true,
      hasFooter = menu.items.nonEmpty,
      itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
      itemTargetRows = targetRows
    )
    val bounds = itemBounds(
      frame,
      window.rowCount,
      hasHeader = true,
      hasFooter = menu.items.nonEmpty,
      state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
      targetRows
    )
    window.slice(menu.items).zip(bounds).zipWithIndex.map {
      case ((item, bound), index) =>
        val selected = window.offset + index == menu.selectedIndex
        AccessibleNode(
          s"surface:${surfaceId.value}/item:${item.id}",
          AccessibilityRole.Button,
          item.label,
          None,
          selected,
          state.persisted.focus == Focus.Surface(surfaceId) && selected,
          bound
        )
    }

  private def modalControls(
    surfaceId: SurfaceId,
    modal: Modal,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val targetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
    ModalSurfaceComposition
      .forModal(modal, frameRect, targetRows)
      .toList
      .flatMap { composition =>
        composition.hitRegions.flatMap { hit =>
          composition.paintBoxes.find(_.focusId.contains(hit.focusId)).map { box =>
            val role = box.kind match
              case SurfacePaintKind.TextInput => AccessibilityRole.TextField
              case _                          => AccessibilityRole.Button
            val value = Option.when(box.kind == SurfacePaintKind.TextInput) {
              box.text.map(_.stripPrefix(hit.semanticLabel).stripPrefix(" ")).getOrElse("")
            }
            AccessibleNode(
              s"surface:${surfaceId.value}/control:${hit.focusId.value}",
              role,
              hit.semanticLabel,
              value,
              box.selected,
              state.persisted.focus == Focus.Surface(surfaceId) && box.selected,
              LayoutRect(hit.rect.x.toInt, hit.rect.y.toInt, hit.rect.width.toInt, hit.rect.height.toInt)
            )
          }
        }
      }

  private def toolbarControls(
    surfaceId: SurfaceId,
    toolbarState: ContextualToolbarState,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val frame = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.ContextualToolbar(toolbarState))
    val targetRows = SurfaceFrameLayout.itemTargetRowsFor(
      SurfaceContent.ContextualToolbar(toolbarState),
      state.persisted.config.interfaceDensity
    )
    val items      = ContextualToolbar.itemsFor(state)
    val normalized = toolbarState.normalized(items)
    val rows       = ContextualToolbarLayout.rowGroups(items, frame.contentRect.width.max(1), normalized.displayMode)
    val rowSlots = frame
      .contentRowSlots(
        itemCount = rows.size,
        hasHeader = false,
        hasFooter = false,
        itemGapRows = state.persisted.config.uiElementGap,
        itemTargetRows = targetRows
      )
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) => index -> y }
      .toMap
    rows.zipWithIndex.flatMap {
      case (row, rowIndex) =>
        val widths = ContextualToolbarLayout.itemCellWidths(row, frame.contentRect.width.max(1), normalized.displayMode)
        val start = frame.contentRect.x + ContextualToolbarLayout.rowLeadingPadding(
          row,
          frame.contentRect.width.max(1),
          normalized.displayMode
        )
        val positions = widths.scanLeft(start)(_ + _ + 1).dropRight(1)
        rowSlots.get(rowIndex).toList.flatMap { y =>
          row.zip(widths).zip(positions).zipWithIndex.map {
            case (((item, width), x), index) =>
              val absoluteIndex = rows.take(rowIndex).map(_.size).sum + index
              val (role, value) = item match
                case ContextualToolbarItem.Input(_, _, _, input) =>
                  AccessibilityRole.TextField -> Some(input.currentValue)
                case ContextualToolbarItem.Dropdown(_, _, _, option) =>
                  AccessibilityRole.Button -> Some(option.selectedOption)
                case _ => AccessibilityRole.Button -> None
              AccessibleNode(
                s"surface:${surfaceId.value}/item:${item.id}",
                role,
                item.label,
                value,
                absoluteIndex == normalized.focusedIndex,
                state.persisted.focus == Focus.Surface(surfaceId) && absoluteIndex == normalized.focusedIndex,
                LayoutRect(x, y, width, targetRows)
              )
          }
        }
    }

  private def pinnedControls(
    surfaceId: SurfaceId,
    content: SurfaceContent,
    frameRect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val frame    = SurfaceFrameLayout.forContent(frameRect, content)
    val resolved = SurfaceContentResolver.resolve(content, frameRect, SurfaceRenderMode.Pinned)
    resolved.rows
      .zip(itemBounds(frame, resolved.rows.size, resolved.header.nonEmpty, resolved.footer.nonEmpty, 0.0))
      .zipWithIndex
      .map {
        case ((row, bound), index) =>
          AccessibleNode(
            s"surface:${surfaceId.value}/item:$index",
            AccessibilityRole.Button,
            row.plainText,
            None,
            row.selected,
            state.persisted.focus == Focus.Surface(surfaceId) && row.selected,
            bound
          )
      }

  private def itemBounds(
    frame: SurfaceFrameLayout,
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double,
    itemTargetRows: Int = 1
  ): List[LayoutRect] =
    frame.contentRowSlots(itemCount, hasHeader, hasFooter, itemGapRows, itemTargetRows).collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) =>
        LayoutRect(frame.contentRect.x, y, frame.contentRect.width, itemTargetRows)
    }

  private def isPinned(presentation: SurfacePresentation): Boolean =
    presentation match
      case SurfacePresentation.Pinned(_, _) | SurfacePresentation.Expanded(_, _) => true
      case _                                                                     => false

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
      case _: SurfaceContent.CommandPalette | _: SurfaceContent.ModalWorkflow =>
        AccessibilityRole.Dialog
      case _: SurfaceContent.CursorInfoBar => AccessibilityRole.Status
      case _                               => AccessibilityRole.Panel

  private def surfaceName(content: SurfaceContent): String =
    content match
      case SurfaceContent.StartPage(page)              => page.title
      case _: SurfaceContent.CommandPalette            => "Command runner"
      case _: SurfaceContent.CommandRunnerPeek         => "Command runner preview"
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
      case SurfaceContent.Comments(_, _)               => "Comments"
      case SurfaceContent.Diagnostics(_, _)            => "Diagnostics"
      case SurfaceContent.ShortcutsHelp(_)             => "Keyboard shortcuts"
      case SurfaceContent.GhostOverlay(original, _)    => surfaceName(original)

  private def surfaceValue(content: SurfaceContent): Option[String] =
    content match
      case SurfaceContent.StartPage(page)     => page.statusMessage
      case SurfaceContent.CursorInfoBar(text) => Some(text)
      case _                                  => None

  private def statusMessage(content: SurfaceContent): Option[String] =
    content match
      case SurfaceContent.CommandPalette(runner) => runner.statusMessage
      case SurfaceContent.ModalWorkflow(modal)   => modalStatusMessage(modal)
      case _                                     => None

  private def modalStatusMessage(modal: Modal): Option[String] =
    modal match
      case Modal.FileWorkflow(workflow)    => workflow.statusMessage
      case Modal.ReplaceWorkflow(workflow) => workflow.statusMessage
      case _                               => None

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
