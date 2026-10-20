package com.serenity.io

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, LinkOption, Path}
import java.util.Locale

import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

import cats.Monad
import cats.effect.IO

/** The files under a project root, relative to it and sorted by path. `truncated` when files are left unlisted: the
  * walk stopped at its limit, or it is still running and this is what it had found so far.
  */
final case class ProjectFileListing(files: Vector[Path], truncated: Boolean)

private[io] enum EntryKind:
  case File, Directory, Other

final private[io] case class DirectoryEntry(name: String, kind: EntryKind)

private[io] type DirectoryReader = Path => Vector[DirectoryEntry]

/** What one directory held when it was last read. `modified` and `listedAt` (epoch millis) let a later visit tell
  * whether anything changed since; `rules` are the directory's own ignore files and `ignoreFiles` their modification
  * times.
  */
final private[io] case class DirectorySnapshot(
    modified: Long,
    listedAt: Long,
    files: Vector[String],
    directories: Vector[String],
    rules: GitIgnore,
    ignoreFiles: Map[String, Long]
)

/** The directories visited breadth-first, nearest the root first, and whether the walk stopped at its limit. */
final private[io] case class Walked(directories: Vector[(Path, DirectorySnapshot)], truncated: Boolean):
  def files: Vector[Path] = directories.flatMap((directory, snapshot) => snapshot.files.map(directory.resolve(_)))

final private[io] case class ExtraRules(rules: IgnoreRules, modified: Map[Path, Long])

/** Lists a project's files for the "Go to File" finder. A file is left out when a `.gitignore` or `.ignore` between it
  * and the root, the repository's `.git/info/exclude`, or a global excludes file ignores it; hidden directories
  * (`.git`, `.idea`, ...), the usual build and dependency output and compiled binaries are skipped regardless, so a
  * project with no `.gitignore` is still not drowned in them. [[ProjectFileIndex]] keeps the result between opens.
  */
object ProjectFileWalker:

  val SkippedDirectories: Set[String] = Set("target", "node_modules", "dist", "build")

  private val SkippedFiles = Set(".git", ".DS_Store")

  private val BinaryExtensions =
    Set(
      "class",
      "jar",
      "war",
      "o",
      "a",
      "so",
      "dylib",
      "dll",
      "exe",
      "pyc",
      "zip",
      "gz",
      "tgz",
      "bz2",
      "xz",
      "7z",
      "tar"
    )

  private[io] val IgnoreFileNames = Vector(".gitignore", ".ignore")

  /** The root as a directory key; the empty path, so that `resolve` of a name gives just that name. */
  private[io] val RootDirectory: Path = Path.of("")

  /** Breadth-first, so a walk cut short at `limit` keeps the files nearest the root. Only the root itself must be
    * readable: an unreadable directory below it is left out rather than failing the whole listing.
    */
  def list(root: Path, limit: Int): IO[ProjectFileListing] =
    IO.blocking(extraRules(root, Vector.empty)).flatMap { extras =>
      build(root, readDirectory, Vector(RootDirectory -> extras.rules), limit, Int.MaxValue, _ => IO.unit).map { walked =>
        ProjectFileListing(walked.files.take(limit).sortBy(_.toString), walked.truncated)
      }
    }

  /** Walks from `starts` (directories relative to `root`, each with the rules inherited from above it). Calls
    * `onProgress` with the files found so far once `firstProgressAt` files are in, and again each time that count
    * doubles, so a huge tree can be shown while it is still being read.
    */
  private[io] def build(
    root: Path,
    read: DirectoryReader,
    starts: Vector[(Path, IgnoreRules)],
    limit: Int,
    firstProgressAt: Int,
    onProgress: Vector[Path] => IO[Unit]
  ): IO[Walked] =
    Monad[IO].tailRecM[Frontier, Walked](Frontier(Queue.from(starts), Vector.empty, 0, firstProgressAt)) { frontier =>
      frontier.pending.dequeueOption match
        case _ if frontier.files > limit => IO.pure(Right(Walked(frontier.done, truncated = true)))
        case None                        => IO.pure(Right(Walked(frontier.done, truncated = false)))
        case Some(((directory, inherited), rest)) =>
          IO.blocking(scan(root, directory, inherited, read)).attempt.flatMap {
            case Left(error) if directory == RootDirectory => IO.raiseError(error)
            case Left(_)                                   => IO.pure(Left(frontier.copy(pending = rest)))
            case Right((snapshot, rules)) =>
              val children = snapshot.directories.map(name => directory.resolve(name) -> rules)
              announce(
                Frontier(
                  rest.enqueueAll(children),
                  frontier.done :+ (directory -> snapshot),
                  frontier.files + snapshot.files.size,
                  frontier.announceAt
                ),
                onProgress
              ).map(Left(_))
          }
    }

  final private case class Frontier(
      pending: Queue[(Path, IgnoreRules)],
      done: Vector[(Path, DirectorySnapshot)],
      files: Int,
      announceAt: Int
  )

  private def announce(frontier: Frontier, onProgress: Vector[Path] => IO[Unit]): IO[Frontier] =
    if frontier.files < frontier.announceAt then IO.pure(frontier)
    else
      onProgress(Walked(frontier.done, truncated = true).files)
        .as(
          frontier.copy(announceAt =
            if frontier.announceAt > Int.MaxValue / 2 then Int.MaxValue else frontier.announceAt * 2
          )
        )

  /** Reads `directory` (relative to `root`) and decides what in it is listed or walked into. Throws when it cannot be
    * read.
    */
  private[io] def scan(
    root: Path,
    directory: Path,
    inherited: IgnoreRules,
    read: DirectoryReader
  ): (DirectorySnapshot, IgnoreRules) =
    val absolute = root.resolve(directory)
    val listedAt = System.currentTimeMillis()
    val modified = Files.getLastModifiedTime(absolute).toMillis
    val entries  = read(absolute).sortBy(_.name)
    val ignoreFiles =
      IgnoreFileNames.filter(name => entries.exists(entry => entry.name == name && entry.kind == EntryKind.File))
    val own                       = GitIgnore(ignoreFiles.flatMap(name => readRules(absolute.resolve(name)).rules))
    val base                      = slashed(directory)
    val rules                     = inherited.withLayer(base, own)
    def projectPath(name: String) = if base.isEmpty then name else s"$base/$name"
    val files = entries
      .filter(entry => entry.kind == EntryKind.File && listedFile(entry.name, projectPath(entry.name), rules))
      .map(_.name)
    val directories = entries
      .filter(entry => entry.kind == EntryKind.Directory && walkedDirectory(entry.name, projectPath(entry.name), rules))
      .map(_.name)
    val stamps = ignoreFiles.map(name => name -> modifiedAt(absolute.resolve(name))).toMap
    (DirectorySnapshot(modified, listedAt, files, directories, own, stamps), rules)

  /** The rules that apply below every directory: the global excludes file(s), then the repository's `info/exclude`,
    * lowest precedence first, and when each was last modified.
    */
  private[io] def extraRules(root: Path, globalExcludes: Vector[Path]): ExtraRules =
    val sources = globalExcludes :+ root.resolve(".git").resolve("info").resolve("exclude")
    ExtraRules(
      sources.foldLeft(IgnoreRules.empty)((rules, source) => rules.withLayer("", readRules(source))),
      sources.map(source => source -> modifiedAt(source)).toMap
    )

  /** The entries of `directory`, classified from the attributes the directory stream already gives access to. A symlink
    * counts as a file when it points to one and is never a directory to walk, so a link back up the tree cannot loop
    * the walk.
    */
  private[io] def readDirectory(directory: Path): Vector[DirectoryEntry] =
    Using.resource(Files.newDirectoryStream(directory))(_.iterator.asScala.map(entryOf).toVector)

  private[io] def slashed(directory: Path): String = directory.toString.replace(File.separatorChar, '/')

  private[io] def modifiedAt(path: Path): Long = Try(Files.getLastModifiedTime(path).toMillis).getOrElse(-1L)

  private def entryOf(path: Path): DirectoryEntry =
    val kind = Try(Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)).toOption
      .fold(EntryKind.Other) { attributes =>
        if attributes.isDirectory then EntryKind.Directory
        else if attributes.isRegularFile || (attributes.isSymbolicLink && Files.isRegularFile(path)) then EntryKind.File
        else EntryKind.Other
      }
    DirectoryEntry(path.getFileName.toString, kind)

  private def readRules(path: Path): GitIgnore =
    Try(Files.readString(path)).map(GitIgnore.parse).getOrElse(GitIgnore.empty)

  private def listedFile(name: String, projectPath: String, rules: IgnoreRules): Boolean =
    !SkippedFiles(name) && !isBinary(name) && !rules.ignores(projectPath, isDirectory = false)

  private def walkedDirectory(name: String, projectPath: String, rules: IgnoreRules): Boolean =
    !name.startsWith(".") && !SkippedDirectories(name) && !rules.ignores(projectPath, isDirectory = true)

  private def isBinary(name: String): Boolean =
    val dot = name.lastIndexOf('.')
    dot > 0 && BinaryExtensions(name.substring(dot + 1).toLowerCase(Locale.ROOT))
