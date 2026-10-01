package com.serenity.state.models

import java.io.File
import java.nio.file.Path
import java.util.Locale

import com.serenity.command.FileFinderCommands
import com.serenity.io.ProjectFileListing
import com.serenity.ui.widget.{Loadable, TextField}

/** The "Go to File" finder: a [[ListPicker]] over the files under a project root, ranked by [[FuzzyMatch]] against what
  * has been typed. Each row is a file's name, with its directory relative to the root as detail.
  */
object FileFinder:

  val Title: String = "Go to File"

  /** The walk stops here, so a huge tree neither stalls the finder nor holds every path it contains in memory. */
  val MaxListedFiles: Int = 20000

  /** Rows past this many are never reached by scrolling in practice; typing more narrows to them instead. */
  val MaxShownChoices: Int = 200

  def picker(root: Path): ListPicker =
    ListPicker(
      Title,
      Loadable.Loading(),
      query = Some(TextField()),
      source = Some(PickerSource.ProjectFiles(root, Loadable.Loading()))
    )

  /** `picker` with its walk of `root` landed, or why it failed, still to be refreshed for the query. */
  def listed(picker: ListPicker, root: Path, listing: Either[String, ProjectFileListing]): ListPicker =
    picker.copy(source = Some(PickerSource.ProjectFiles(root, listing.fold(Loadable.Failed(_), Loadable.Ready(_)))))

  /** The finder's rows for its current query. With nothing typed, files recently opened come first, then the rest by
    * path; otherwise the files matching the query, best first.
    */
  def refreshed(
    picker: ListPicker,
    root: Path,
    listing: Loadable[ProjectFileListing],
    recentFiles: List[Path]
  ): ListPicker =
    listing match
      case Loadable.Ready(project) =>
        val titled = picker.copy(title = if project.truncated then truncatedTitle else Title)
        if project.files.isEmpty then titled.copy(items = Loadable.Empty(s"No files under $root"))
        else
          val shown =
            if picker.queryText.isEmpty then recentFirst(project.files, root, recentFiles)
            else ranked(project.files, picker.queryText)
          titled.withChoices(shown.take(MaxShownChoices).map(choice(root, _)), ListPicker.NoMatches)
      case Loadable.Loading(progress) => picker.copy(items = Loadable.Loading(progress))
      case Loadable.Empty(message)    => picker.copy(items = Loadable.Empty(message))
      case Loadable.Failed(reason)    => picker.copy(items = Loadable.Failed(reason))

  /** The root a docked explorer shows -- where "Open as root" points the app -- if one is docked. */
  def explorerRoot(state: AppState): Option[Path] =
    state.runtime.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.DirectoryTree(tree, _), SurfacePresentation.Docked, _) => tree.rootPath
    }

  private def truncatedTitle: String = s"$Title (first ${"%,d".formatLocal(Locale.ROOT, MaxListedFiles)} files)"

  private def ranked(files: Vector[Path], query: String): Vector[Path] =
    files
      .flatMap(file => FuzzyMatch.score(query, slashed(file)).map(file -> _))
      .sortBy((file, score) => (-score, file.toString))
      .map(_._1)

  private def recentFirst(files: Vector[Path], root: Path, recentFiles: List[Path]): Vector[Path] =
    val base   = root.normalize()
    val listed = files.toSet
    val recent = recentFiles
      .map(_.normalize())
      .filter(_.startsWith(base))
      .map(base.relativize)
      .filter(listed)
      .distinct
      .toVector
    val recentSet = recent.toSet
    recent ++ files.filterNot(recentSet)

  private def choice(root: Path, file: Path): ListChoice =
    ListChoice(
      Option(file.getFileName).fold(slashed(file))(_.toString),
      Option(file.getParent).map(slashed),
      FileFinderCommands.openFile(root.resolve(file))
    )

  private def slashed(path: Path): String = path.toString.replace(File.separatorChar, '/')
