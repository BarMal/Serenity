package com.serenity.command.menu

import com.serenity.command.{CommandId, CommandRegistry}
import com.serenity.config.AppMode
import com.serenity.state.models.Shell
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ui.menu_bar = hidden` is only free if the palette offers everything the menus do. */
class MenuPaletteReachabilitySpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.withToggleUI

  private val specs = List(
    "Linux"    -> MenuSpec.forOs("Linux"),
    "Windows"  -> MenuSpec.forOs("Windows 11"),
    "combined" -> MenuSpec.forOs("Linux", fileOrFolderOpen = true)
  )

  private def itemIds(entries: List[MenuEntry]): List[CommandId] =
    entries.flatMap:
      case MenuEntry.Item(id)             => List(id)
      case MenuEntry.Submenu(_, children) => itemIds(children)
      case _                              => Nil

  private def paletteNames(mode: AppMode): Set[String] =
    registry.availableCommands(mode, Shell.Gui).map(_.name).toSet

  "Every MenuSpec command id" should "resolve in the registry the palette lists from" in {
    for (name, spec) <- specs; (_, entries) <- spec.menus; id <- itemIds(entries) do
      withClue(s"$name menu id ${id.value}: ")(registry.findCommand(id.value) should not be empty)
  }

  it should "be offered by the palette in at least one mode of the window" in {
    val offered = AppMode.values.toList.flatMap(mode => paletteNames(mode))
    for (name, spec) <- specs; (_, entries) <- spec.menus; id <- itemIds(entries) do
      withClue(s"$name menu id ${id.value}: ")(offered should contain(id.value))
  }

  "Every command a menu shows in a mode" should "be offered by the palette in that mode" in {
    for
      (name, spec) <- specs
      mode         <- AppMode.values.toList
      menu         <- MenuModel.resolve(spec, registry, mode, Shell.Gui)
      id           <- resolvedIds(menu.entries)
    do withClue(s"$name, $mode, $id: ")(paletteNames(mode) should contain(id))
  }

  private def resolvedIds(entries: List[ResolvedEntry]): List[String] =
    entries.flatMap:
      case ResolvedEntry.Item(command)       => List(command.name)
      case ResolvedEntry.Submenu(_, content) => resolvedIds(content)
      case _                                 => Nil
