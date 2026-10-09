package com.serenity.command.menu

import java.nio.file.Path

import com.serenity.command.menu.MenuDispatch.Choice
import com.serenity.command.{Command, CommandRegistry}
import com.serenity.state.models.{AppState, StartupPageContent, TabListContent, TabListEntry}

/** What a [[DynamicSource]] stands for in a given model, with no Swing in it. */
object DynamicMenu:

  enum Item:

    case Choose(
        label: String,
        description: Option[String],
        choice: Choice,
        checked: Boolean,
        mnemonic: Option[MenuMnemonics.Mnemonic]
    )

    case Placeholder(label: String)
    case Run(command: Command)
    case Separator

  /** `submenuTitle` is where the items nest; without one they sit inline in the menu that names the source. */
  final case class Section(submenuTitle: Option[String], items: List[Item])

  val RecentFilesLimit: Int = StartupPageContent.RecentFilesLimit

  val OpenRecentTitle: String = "Open Recent"
  val NoRecentFiles: String   = "(No recent files)"

  private val clearRecentFiles = "clear-recent-files"

  /** The tab bar's dirty glyph, which `TabBarSurfaceComposition` keeps private. */
  private val DirtyGlyph = " ●"

  private val NumberedWindows = 9

  def expand(source: DynamicSource, app: AppState, registry: CommandRegistry): Section =
    source match
      case DynamicSource.RecentFiles => Section(Some(OpenRecentTitle), recentFiles(app, registry))
      case DynamicSource.OpenBuffers => Section(None, openBuffers(app))

  private def recentFiles(app: AppState, registry: CommandRegistry): List[Item] =
    val paths = app.persisted.recentFiles.map(_.toAbsolutePath.normalize).distinct.take(RecentFilesLimit)
    if paths.isEmpty then List(Item.Placeholder(NoRecentFiles))
    else
      paths.map(recentItem) ++ registry
        .findCommand(clearRecentFiles)
        .toList
        .flatMap(c => List(Item.Separator, Item.Run(c)))

  private def recentItem(path: Path): Item =
    Item.Choose(
      label = Option(path.getFileName).getOrElse(path).toString,
      description = Option(path.getParent).map(_.toString),
      choice = Choice.RecentFile(path),
      checked = false,
      mnemonic = None
    )

  private def openBuffers(app: AppState): List[Item] =
    val tabs = TabListContent.build(app)
    tabs.entries.zipWithIndex.map: (entry, index) =>
      windowItem(entry, index, tabs.activeBufferId.contains(entry.bufferId), app)

  private def windowItem(entry: TabListEntry, index: Int, active: Boolean, app: AppState): Item =
    val number = Option.when(index < NumberedWindows)(('1' + index).toChar)
    val title  = if entry.isDirty then s"${entry.title}$DirtyGlyph" else entry.title
    val path   = app.persisted.buffers.get(entry.bufferId).flatMap(_.document.filePath).map(_.toString)
    Item.Choose(
      label = number.fold(title)(n => s"$n $title"),
      description = windowDescription(path, entry.isDirty),
      choice = Choice.Buffer(entry.bufferId),
      checked = active,
      mnemonic = number.map(MenuMnemonics.Mnemonic(0, _))
    )

  private def windowDescription(path: Option[String], dirty: Boolean): Option[String] =
    (path, dirty) match
      case (Some(shown), true) => Some(s"$shown (unsaved changes)")
      case (Some(shown), _)    => Some(shown)
      case (None, true)        => Some("Unsaved changes")
      case (None, false)       => None
