package com.serenity.command.menu

import java.nio.file.Paths

import com.serenity.command.{CommandId, CommandRegistry}
import com.serenity.config.AppMode
import com.serenity.keystroke.events.{
  ActivateBuffer,
  Copy,
  Direction,
  FocusInDirection,
  OpenFile,
  OpenRecentFolder,
  OpenRecentPath,
  Paste,
  Redo,
  RunCommand,
  SaveFile
}
import com.serenity.state.models.{BufferId, Shell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuDispatchSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.withToggleUI

  "A command item" should "send the event its hotkey action sends, so a menu Paste is Ctrl+V" in {
    MenuDispatch.eventFor(CommandId("paste")) shouldBe Paste
    MenuDispatch.eventFor(CommandId("copy")) shouldBe Copy
    MenuDispatch.eventFor(CommandId("redo")) shouldBe Redo
    MenuDispatch.eventFor(CommandId("save")) shouldBe SaveFile
    MenuDispatch.eventFor(CommandId("open")) shouldBe OpenFile
  }

  it should "send RunCommand when no hotkey action sends an event for the command" in {
    MenuDispatch.eventFor(CommandId("toggle-line-numbers")) shouldBe RunCommand("toggle-line-numbers")
    MenuDispatch.eventFor(CommandId("save-session")) shouldBe RunCommand("save-session")
  }

  it should "send the focus event for a focus command, as its key does" in {
    MenuDispatch.eventFor(CommandId("focus-left")) shouldBe FocusInDirection(Direction.Left)
  }

  "A dynamic choice" should "activate the buffer or open the recent path" in {
    val path = Paths.get("/tmp/draft.md")

    MenuDispatch.eventFor(MenuDispatch.Choice.Buffer(BufferId(3))) shouldBe ActivateBuffer(BufferId(3))
    MenuDispatch.eventFor(MenuDispatch.Choice.RecentFile(path)) shouldBe OpenRecentPath(path)
  }

  it should "open a recent folder as the project root" in {
    val path = Paths.get("/tmp/book")

    MenuDispatch.eventFor(MenuDispatch.Choice.RecentFolder(path)) shouldBe OpenRecentFolder(path)
  }

  "Every resolved menu entry" should "map to an event only when choosing it needs no further input" in {
    val resolved = MenuModel.resolve(MenuSpec.forOs("Linux"), registry, AppMode.Code, Shell.Gui)
    val entries  = resolved.flatMap(menu => flatten(menu.entries))
    val items    = entries.collect { case item: ResolvedEntry.Item => item }

    items should not be empty
    items.foreach { item =>
      MenuDispatch.eventFor(item) shouldBe Some(MenuDispatch.eventFor(CommandId(item.command.name)))
    }
    entries.filterNot(items.contains).foreach(entry => MenuDispatch.eventFor(entry) shouldBe None)
    entries.collect { case ResolvedEntry.Dynamic(source) => source }.toSet shouldBe
      Set(DynamicSource.RecentFiles, DynamicSource.RecentFolders, DynamicSource.OpenBuffers)
  }

  private def flatten(entries: List[ResolvedEntry]): List[ResolvedEntry] =
    entries.flatMap:
      case sub: ResolvedEntry.Submenu => sub :: flatten(sub.entries)
      case other                      => List(other)
