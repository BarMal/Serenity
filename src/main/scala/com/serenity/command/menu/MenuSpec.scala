package com.serenity.command.menu

import com.serenity.command.CommandId

final case class MenuSpec(menus: List[(MenuTitle, List[MenuEntry])])

object MenuSpec:

  /** On macOS, quit and settings belong to the application menu, which the platform builds. */
  def forOs(osName: String, fileOrFolderOpen: Boolean = false): MenuSpec =
    val isMac = osName.toLowerCase(java.util.Locale.ROOT).contains("mac")
    MenuSpec(
      List(
        MenuTitle.File   -> file(isMac, fileOrFolderOpen),
        MenuTitle.Edit   -> edit,
        MenuTitle.View   -> view,
        MenuTitle.Window -> window(isMac),
        MenuTitle.Help   -> help
      )
    )

  private def items(names: String*): List[MenuEntry] = names.toList.map(name => MenuEntry.Item(CommandId(name)))

  private def file(isMac: Boolean, fileOrFolderOpen: Boolean): List[MenuEntry] =
    (if fileOrFolderOpen then items("new", "open-file-or-folder") else items("new", "open", "open-folder")) ++ List(
      MenuEntry.Dynamic(DynamicSource.RecentFiles)
    ) ++ items(
      "go-to-file",
      "file-search"
    ) ++
      List(MenuEntry.Separator) ++ items("save", "save-as") ++
      List(MenuEntry.Separator) ++ items("close", "close-others", "close-all") ++
      List(MenuEntry.Separator, session) ++ items("return-to-start-page") ++
      (if isMac then Nil else List(MenuEntry.Separator) ++ items("open-settings", "quit"))

  private def session: MenuEntry =
    MenuEntry.Submenu(
      MenuTitle.Session,
      items("save-session", "save-session-as", "open-session", "rename-session", "restore-session", "clear-session")
    )

  private def edit: List[MenuEntry] =
    items("undo", "redo") ++ List(MenuEntry.Separator) ++
      items("cut", "copy", "paste", "paste-from-history", "select-all") ++ List(MenuEntry.Separator) ++
      items("find", "find-all", "replace", "replace-all", "goto-line") ++ List(MenuEntry.Separator) ++
      items("cut-to-darlings", "restore-darling")

  private def view: List[MenuEntry] =
    items(
      "toggle-line-numbers",
      "toggle-line-wrap",
      "toggle-status-line",
      "toggle-pane-headers",
      "toggle-typewriter-scrolling",
      "toggle-column-mode",
      "toggle-text-body-focus",
      "toggle-contextual-toolbar"
    ) ++ List(MenuEntry.Separator) ++ items("theme-chooser", "markdown-preview")

  private def window(isMac: Boolean): List[MenuEntry] =
    (if isMac then
       List(MenuEntry.Platform(PlatformAction.Minimize), MenuEntry.Platform(PlatformAction.Zoom), MenuEntry.Separator)
     else Nil) ++
      items(
        "split-pane-horizontal",
        "split-pane-vertical",
        "close-pane",
        "toggle-maximise-panel",
        "next-tab",
        "previous-tab"
      ) ++
      List(MenuEntry.Separator, MenuEntry.Dynamic(DynamicSource.OpenBuffers))

  private def help: List[MenuEntry] = items("toggle-shortcuts-help", "show-licence-and-notices")
