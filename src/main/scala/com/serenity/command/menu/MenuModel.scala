package com.serenity.command.menu

import com.serenity.command.{Command, CommandRegistry, scope}
import com.serenity.config.AppMode
import com.serenity.state.models.Shell

enum ResolvedEntry:
  case Item(command: Command)
  case Submenu(title: MenuTitle, entries: List[ResolvedEntry])
  case Dynamic(source: DynamicSource)
  case Platform(action: PlatformAction)
  case Separator

final case class ResolvedMenu(title: MenuTitle, entries: List[ResolvedEntry])

object MenuModel:

  /** Looks each id up in `registry`, leaving out what `mode` and `shell` do not offer (the palette's hidden commands),
    * then the separators and menus that leaves with nothing to separate.
    */
  def resolve(spec: MenuSpec, registry: CommandRegistry, mode: AppMode, shell: Shell): List[ResolvedMenu] =
    spec.menus.flatMap: (title, entries) =>
      val resolved = tidy(entries.flatMap(resolveEntry(_, registry, mode, shell)))
      Option.when(resolved.nonEmpty)(ResolvedMenu(title, resolved))

  private def resolveEntry(
    entry: MenuEntry,
    registry: CommandRegistry,
    mode: AppMode,
    shell: Shell
  ): Option[ResolvedEntry] =
    entry match
      case MenuEntry.Item(id) =>
        registry.findCommand(id.value).filter(_.scope.admits(mode, shell)).map(ResolvedEntry.Item(_))
      case MenuEntry.Submenu(title, children) =>
        Some(tidy(children.flatMap(resolveEntry(_, registry, mode, shell))))
          .filter(_.nonEmpty)
          .map(ResolvedEntry.Submenu(title, _))
      case MenuEntry.Dynamic(source)  => Some(ResolvedEntry.Dynamic(source))
      case MenuEntry.Platform(action) => Some(ResolvedEntry.Platform(action))
      case MenuEntry.Separator        => Some(ResolvedEntry.Separator)

  private def tidy(entries: List[ResolvedEntry]): List[ResolvedEntry] =
    val withoutDoubled = entries.foldRight(List.empty[ResolvedEntry]):
      case (ResolvedEntry.Separator, rest @ (ResolvedEntry.Separator :: _)) => rest
      case (entry, rest)                                                    => entry :: rest
    withoutDoubled.dropWhile(isSeparator).reverse.dropWhile(isSeparator).reverse

  private def isSeparator(entry: ResolvedEntry): Boolean = entry == ResolvedEntry.Separator
