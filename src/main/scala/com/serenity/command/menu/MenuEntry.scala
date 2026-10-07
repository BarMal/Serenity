package com.serenity.command.menu

import com.serenity.command.CommandId

/** The only menu text that is not a command's own label. */
enum MenuTitle(val text: String):
  case File    extends MenuTitle("File")
  case Edit    extends MenuTitle("Edit")
  case View    extends MenuTitle("View")
  case Window  extends MenuTitle("Window")
  case Help    extends MenuTitle("Help")
  case Session extends MenuTitle("Session")

/** Menu content that depends on the running session, so the shell fills it in when the menu opens. */
enum DynamicSource:
  case RecentFiles
  case OpenBuffers

/** Actions the window manager performs, so they are not commands. */
enum PlatformAction:
  case Minimize
  case Zoom
  case BringAllToFront

/** A menu names its commands by id alone: labels and keys always come from the registry and the keymap. */
enum MenuEntry:
  case Item(id: CommandId)
  case Submenu(title: MenuTitle, entries: List[MenuEntry])
  case Dynamic(source: DynamicSource)
  case Platform(action: PlatformAction)
  case Separator
