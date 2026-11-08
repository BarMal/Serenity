package com.serenity.lsp.client

import java.net.URI
import java.nio.file.{Path, Paths}

import scala.util.Try

import io.circe.{ACursor, Json}

/** The data of LSP 3.17 `workspace/didChangeWatchedFiles`: what a server registers for and what it is sent (#2531). */
object LspWatch:

  val Method: String = "workspace/didChangeWatchedFiles"

  /** The spec's `FileChangeType`; `code` is its number on the wire. */
  enum FileChangeType(val code: Int):
    case Created extends FileChangeType(1)
    case Changed extends FileChangeType(2)
    case Deleted extends FileChangeType(3)

  final case class WatchedFileChange(path: Path, change: FileChangeType)

  enum WatchedGlob:
    /** A plain pattern. The spec does not say what it is matched against, so a path matches when either its path
      * relative to the server's root or its absolute path does: servers send both root-relative globs and absolute
      * paths.
      */
    case Anywhere(glob: LspGlob)

    /** A `RelativePattern`: matched against the path relative to `base`, and only below it. */
    case Under(base: Path, glob: LspGlob)

    def matches(path: Path, root: Path): Boolean = this match
      case Anywhere(glob) =>
        glob.matches(slashed(path)) || (path.startsWith(root) && glob.matches(slashed(root.relativize(path))))
      case Under(base, glob) => path.startsWith(base) && glob.matches(slashed(base.relativize(path)))

  final case class FileSystemWatcher(glob: WatchedGlob, kinds: Set[FileChangeType])

  /** The watchers of every `didChangeWatchedFiles` entry in `client/registerCapability` params, by registration id.
    * Other methods are not this object's to read, and a watcher that cannot be read is skipped rather than failing its
    * whole registration.
    */
  def parseRegistrations(params: Json): List[(String, List[FileSystemWatcher])] =
    entriesFor(params.hcursor.downField("registrations")).map { entry =>
      val watchers = entry.downField("registerOptions").downField("watchers").values.toList.flatten
      id(entry) -> watchers.flatMap(watcher => fileSystemWatcher(watcher.hcursor))
    }

  /** The ids of every withdrawn `didChangeWatchedFiles` registration. The field is `unregisterations` in the spec. */
  def parseUnregistrations(params: Json): List[String] =
    entriesFor(params.hcursor.downField("unregisterations")).map(id)

  def didChangeWatchedFilesParams(changes: Vector[WatchedFileChange]): Json =
    Json.obj(
      "changes" -> Json.fromValues(changes.map { change =>
        Json.obj("uri" -> Json.fromString(change.path.toUri.toString), "type" -> Json.fromInt(change.change.code))
      })
    )

  private val AllKinds = 7

  private def entriesFor(list: ACursor): List[ACursor] =
    list.values.toList.flatten
      .map(_.hcursor)
      .filter(entry =>
        entry.downField("method").as[String].contains(Method) && entry.downField("id").as[String].isRight
      )

  private def id(entry: ACursor): String = entry.downField("id").as[String].getOrElse("")

  private def fileSystemWatcher(watcher: ACursor): Option[FileSystemWatcher] =
    val kinds = watcher.downField("kind").as[Int].getOrElse(AllKinds)
    globPattern(watcher.downField("globPattern")).map(
      FileSystemWatcher(_, FileChangeType.values.toSet.filter(kind => (kinds & (1 << (kind.code - 1))) != 0))
    )

  private def globPattern(pattern: ACursor): Option[WatchedGlob] =
    pattern.as[String].toOption.map(text => WatchedGlob.Anywhere(LspGlob.compile(text))).orElse {
      val base = pattern.downField("baseUri")
      for
        uri  <- base.as[String].toOption.orElse(base.downField("uri").as[String].toOption)
        path <- Try(Paths.get(URI.create(uri))).toOption
        glob <- pattern.downField("pattern").as[String].toOption
      yield WatchedGlob.Under(path, LspGlob.compile(glob))
    }

  private def slashed(path: Path): String = path.toString.replace('\\', '/')
