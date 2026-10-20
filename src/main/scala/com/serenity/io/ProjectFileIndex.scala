package com.serenity.io

import java.nio.file.{Files, Path}

import scala.util.Try

import cats.Monad
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.io.ProjectFileWalker.{RootDirectory, extraRules, scan, slashed}

private[io] type Snapshots = Map[Path, DirectorySnapshot]

/** Every directory of a project as it was last read, and when the rules that applied outside the project's own ignore
  * files were.
  */
final private[io] case class ProjectTree(directories: Snapshots, extras: Map[Path, Long]):

  def listing: ProjectFileListing =
    ProjectFileListing(
      directories.iterator
        .flatMap((directory, snapshot) => snapshot.files.map(directory.resolve(_)))
        .toVector
        .sortBy(_.toString),
      truncated = false
    )

/** The files of the projects the "Go to File" finder has been opened on (#1892). A project is walked once; each later
  * request only checks which directories changed since and reads those again, so reopening the finder on a large tree
  * costs one stat per directory rather than a full walk. A directory has changed when its modification time differs
  * from the one recorded, when it was last read too soon after its last change for the time to be trusted (coarse
  * filesystem clocks), or when an ignore file in it was edited; a changed ignore file re-walks that directory's
  * subtree, since it may include or exclude anything below.
  */
final class ProjectFileIndex private (
    trees: Ref[IO, Map[Path, ProjectTree]],
    read: DirectoryReader,
    globalExcludes: IO[Vector[Path]],
    firstPartialAt: Int
):
  import ProjectFileIndex.{MaxProjects, RacyWindowMillis}

  /** The files under `root`. While a first walk is still running `onPartial` receives the files found so far (marked
    * `truncated`), so a huge tree can be shown and searched before the walk ends.
    */
  def listing(root: Path, onPartial: ProjectFileListing => IO[Unit]): IO[ProjectFileListing] =
    val key = root.toAbsolutePath.normalize()
    trees.get
      .map(_.get(key))
      .flatMap(_.fold(walked(key, onPartial))(refreshed(key, _, onPartial)))
      .flatMap(tree => remember(key, tree).as(tree.listing))
      .onError { case _ => trees.update(_ - key) }

  private def walked(root: Path, onPartial: ProjectFileListing => IO[Unit]): IO[ProjectTree] =
    for
      global <- globalExcludes
      extras <- IO.blocking(extraRules(root, global))
      walk <- ProjectFileWalker.build(
        root,
        read,
        Vector(RootDirectory -> extras.rules),
        Int.MaxValue,
        firstPartialAt,
        found => onPartial(ProjectFileListing(found.sortBy(_.toString), truncated = true))
      )
    yield ProjectTree(walk.directories.toMap, extras.modified)

  private def refreshed(root: Path, tree: ProjectTree, onPartial: ProjectFileListing => IO[Unit]): IO[ProjectTree] =
    for
      global <- globalExcludes
      extras <- IO.blocking(extraRules(root, global))
      current <-
        if extras.modified != tree.extras then walked(root, onPartial)
        else revalidated(root, tree.directories, extras.rules).map(ProjectTree(_, tree.extras))
    yield current

  /** Visits the tree level by level, reading again just the directories that changed. */
  private def revalidated(
    root: Path,
    snapshots: Snapshots,
    base: IgnoreRules
  ): IO[Snapshots] =
    Monad[IO].tailRecM[(Snapshots, Vector[Path]), Snapshots]((snapshots, Vector(RootDirectory))) { (directories, level) =>
      if level.isEmpty then IO.pure(Right(directories))
      else
        IO.blocking(level.partition(directory => directories.get(directory).forall(changed(root, directory, _))))
          .flatMap { (changedDirectories, unchanged) =>
            val below = unchanged.flatMap(directory => subdirectories(directories, directory))
            changedDirectories
              .foldLeftM((directories, below)) { (state, directory) =>
                reread(root, base, directory, state._1).map((updated, more) => (updated, state._2 ++ more))
              }
              .map(Left(_))
          }
    }

  /** Reads `directory` again. Returns the updated snapshots and the subdirectories still to be checked: those that were
    * there before and are not new, since a new one is walked here and then needs no check.
    */
  private def reread(
    root: Path,
    base: IgnoreRules,
    directory: Path,
    snapshots: Snapshots
  ): IO[(Snapshots, Vector[Path])] =
    snapshots.get(directory).fold(IO.pure((snapshots, Vector.empty[Path]))) { before =>
      IO.blocking(scan(root, directory, rulesAbove(directory, snapshots, base), read)).attempt.flatMap {
        case Left(error) if directory == RootDirectory => IO.raiseError(error)
        case Left(_)                                   => IO.pure((without(snapshots, Vector(directory)), Vector.empty))
        case Right((after, rules)) =>
          val rulesChanged = after.rules != before.rules
          val gone =
            if rulesChanged then before.directories else before.directories.filterNot(after.directories.contains)
          val added =
            if rulesChanged then after.directories else after.directories.filterNot(before.directories.contains)
          val kept    = after.directories.filterNot(added.contains)
          val trimmed = without(snapshots, gone.map(directory.resolve(_))) + (directory -> after)
          ProjectFileWalker
            .build(
              root,
              read,
              added.map(name => directory.resolve(name) -> rules),
              Int.MaxValue,
              Int.MaxValue,
              _ => IO.unit
            )
            .map(walk => (trimmed ++ walk.directories, kept.map(directory.resolve(_))))
      }
    }

  private def changed(root: Path, directory: Path, snapshot: DirectorySnapshot): Boolean =
    Try {
      val absolute = root.resolve(directory)
      snapshot.modified > snapshot.listedAt - RacyWindowMillis ||
      Files.getLastModifiedTime(absolute).toMillis != snapshot.modified ||
      snapshot.ignoreFiles.exists((name, recorded) => ProjectFileWalker.modifiedAt(absolute.resolve(name)) != recorded)
    }.getOrElse(true)

  private def subdirectories(snapshots: Snapshots, directory: Path): Vector[Path] =
    snapshots.get(directory).toVector.flatMap(_.directories.map(directory.resolve(_)))

  private def rulesAbove(directory: Path, snapshots: Snapshots, base: IgnoreRules): IgnoreRules =
    ProjectFileIndex.ancestors(directory).foldLeft(base) { (rules, ancestor) =>
      snapshots.get(ancestor).fold(rules)(snapshot => rules.withLayer(slashed(ancestor), snapshot.rules))
    }

  private def without(snapshots: Snapshots, roots: Vector[Path]): Snapshots =
    snapshots.filterNot((path, _) => roots.exists(removed => path.startsWith(removed)))

  private def remember(root: Path, tree: ProjectTree): IO[Unit] =
    trees.update(all => all.removedAll(all.keys.filter(_ != root).drop(MaxProjects - 1)) + (root -> tree))

object ProjectFileIndex:

  /** Projects kept at once; opening the finder on another evicts one of the others. */
  val MaxProjects: Int = 4

  /** A directory last read this soon after its last change cannot be told from one that changed again within the same
    * clock tick, so it is read again; two seconds covers FAT's granularity.
    */
  val RacyWindowMillis: Long = 2000L

  val DefaultFirstPartialAt: Int = 4096

  /** An index holding nothing yet. `Ref.unsafe` only allocates, so it is safe to build where no `IO` is at hand. */
  def empty(
    globalExcludes: IO[Vector[Path]] = gitGlobalExcludes,
    firstPartialAt: Int = DefaultFirstPartialAt
  ): ProjectFileIndex =
    emptyReading(ProjectFileWalker.readDirectory, globalExcludes, firstPartialAt)

  private[io] def emptyReading(
    read: DirectoryReader,
    globalExcludes: IO[Vector[Path]],
    firstPartialAt: Int
  ): ProjectFileIndex =
    new ProjectFileIndex(Ref.unsafe[IO, Map[Path, ProjectTree]](Map.empty), read, globalExcludes, firstPartialAt)

  /** The excludes file git itself would apply: `core.excludesFile` from the user's git config, else the default
    * `$XDG_CONFIG_HOME/git/ignore` (`~/.config/git/ignore`).
    */
  val gitGlobalExcludes: IO[Vector[Path]] =
    IO.blocking {
      val home   = Path.of(sys.props.getOrElse("user.home", "."))
      val config = sys.env.get("XDG_CONFIG_HOME").filter(_.nonEmpty).fold(home.resolve(".config"))(Path.of(_))
      val setting = Vector(home.resolve(".gitconfig"), config.resolve("git").resolve("config")).flatMap { file =>
        Try(Files.readString(file)).toOption.flatMap(GitIgnore.excludesFileFrom(_, home))
      }
      if setting.nonEmpty then setting.take(1) else Vector(config.resolve("git").resolve("ignore"))
    }

  /** The directories above `directory`, from the root down; none for the root itself. */
  private[io] def ancestors(directory: Path): Vector[Path] =
    if directory == RootDirectory then Vector.empty
    else
      Vector.tabulate(directory.getNameCount)(depth =>
        if depth == 0 then RootDirectory else directory.subpath(0, depth)
      )
