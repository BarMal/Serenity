package com.serenity.command.menu

import com.serenity.command.{CommandId, CommandKeyBindings, CommandRegistry, scope}
import com.serenity.config.{AppMode, HotkeyAction}
import com.serenity.state.models.Shell
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuModelSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.withToggleUI
  private val linux    = MenuSpec.forOs("Linux")
  private val mac      = MenuSpec.forOs("Mac OS X")

  private def itemIds(entries: List[MenuEntry]): List[CommandId] =
    entries.flatMap:
      case MenuEntry.Item(id)             => List(id)
      case MenuEntry.Submenu(_, children) => itemIds(children)
      case _                              => Nil

  private def specIds(spec: MenuSpec): List[CommandId] = spec.menus.flatMap((_, entries) => itemIds(entries))

  private def resolvedNames(entries: List[ResolvedEntry]): List[String] =
    entries.flatMap:
      case ResolvedEntry.Item(command)       => List(command.name)
      case ResolvedEntry.Submenu(_, content) => resolvedNames(content)
      case _                                 => Nil

  private def namesFor(mode: AppMode): List[String] =
    MenuModel.resolve(linux, registry, mode, Shell.Gui).flatMap(menu => resolvedNames(menu.entries))

  "Every menu item id" should "name a command in the live registry" in {
    for spec <- List(linux, mac); id <- specIds(spec) do registry.isRegistered(id) shouldBe true
  }

  it should "appear once in a menu bar" in {
    for spec <- List(linux, mac) do specIds(spec).distinct shouldBe specIds(spec)
  }

  "The File menu" should "list Open File and Open Folder next to each other, on every platform" in {
    for spec <- List(linux, mac) do
      val ids = specIds(spec).map(_.value)
      ids.indexOf("open-folder") shouldBe ids.indexOf("open") + 1
  }

  it should "offer Open Folder in both modes" in {
    namesFor(AppMode.Code) should contain("open-folder")
    namesFor(AppMode.Prose) should contain("open-folder")
  }

  it should "offer one Open... in place of Open File and Open Folder where the dialog takes either" in {
    val combined = MenuSpec.forOs("Mac OS X", fileOrFolderOpen = true)
    val ids      = specIds(combined).map(_.value)

    ids.indexOf("open-file-or-folder") shouldBe ids.indexOf("new") + 1
    ids should not contain "open"
    ids should not contain "open-folder"
    specIds(combined).foreach(id => registry.isRegistered(id) shouldBe true)
  }

  it should "not offer Open... where the dialog cannot take either" in {
    for spec <- List(linux, mac) do specIds(spec).map(_.value) should not contain "open-file-or-folder"
  }

  "The macOS menu bar" should "leave quit and open-settings to the application menu" in {
    specIds(mac).map(_.value).toSet.intersect(Set("quit", "open-settings", "about")) shouldBe empty
    specIds(linux).map(_.value) should contain allOf ("quit", "open-settings")
  }

  it should "offer the window manager actions the others lack" in {
    def platformActions(spec: MenuSpec): List[PlatformAction] =
      spec.menus.flatMap((_, entries) => entries).collect { case MenuEntry.Platform(action) => action }

    platformActions(mac) shouldBe List(PlatformAction.Minimize, PlatformAction.Zoom)
    platformActions(linux) shouldBe Nil
  }

  "A resolved menu" should "keep the spec's menus and the registry's commands" in {
    val resolved = MenuModel.resolve(linux, registry, AppMode.Prose, Shell.Gui)

    resolved.map(_.title) shouldBe linux.menus.map(_._1)
    resolved.flatMap(menu => resolvedNames(menu.entries)) shouldBe specIds(linux).map(_.value)
  }

  it should "drop the commands the mode does not offer" in {
    val inCode  = namesFor(AppMode.Code)
    val inProse = namesFor(AppMode.Prose)

    inProse.toSet.diff(inCode.toSet) shouldBe Set("cut-to-darlings", "restore-darling")
    inCode.foreach: name =>
      registry.findCommand(name).exists(_.scope.admits(AppMode.Code, Shell.Gui)) shouldBe true
  }

  it should "collapse leading, trailing and doubled separators" in {
    val spec = MenuSpec(
      List(
        MenuTitle.File -> List(
          MenuEntry.Separator,
          MenuEntry.Item(CommandId("save")),
          MenuEntry.Separator,
          MenuEntry.Item(CommandId("not-a-command")),
          MenuEntry.Separator,
          MenuEntry.Item(CommandId("close")),
          MenuEntry.Separator
        ),
        MenuTitle.Help -> List(MenuEntry.Item(CommandId("not-a-command")), MenuEntry.Separator)
      )
    )

    val resolved = MenuModel.resolve(spec, registry, AppMode.Code, Shell.Gui)

    resolved.map(_.title) shouldBe List(MenuTitle.File)
    resolved.flatMap(_.entries).map {
      case ResolvedEntry.Item(command) => command.name
      case ResolvedEntry.Separator     => "-"
      case other                       => fail(s"unexpected $other")
    } shouldBe List("save", "-", "close")
  }

  it should "drop a submenu that ends up empty" in {
    val spec = MenuSpec(
      List(
        MenuTitle.File -> List(
          MenuEntry.Item(CommandId("save")),
          MenuEntry.Submenu(MenuTitle.Session, List(MenuEntry.Item(CommandId("not-a-command"))))
        )
      )
    )

    MenuModel.resolve(spec, registry, AppMode.Code, Shell.Gui).flatMap(_.entries).size shouldBe 1
  }

  "The menu bar" should "reach every command a hotkey action performs, bar those named here" in {
    val reached = specIds(linux).toSet
    val unreached = HotkeyAction.values.toList
      .flatMap(CommandKeyBindings.commandFor)
      .filterNot(reached.contains)
      .map(_.value)

    unreached.toSet shouldBe Set(
      "focus-left",
      "focus-right",
      "focus-up",
      "focus-down",
      "toggle-chapter-ghosts",
      "open-chapter-note",
      "toggle-notes-pin"
    )
  }
