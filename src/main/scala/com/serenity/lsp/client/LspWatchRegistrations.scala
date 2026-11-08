package com.serenity.lsp.client

import java.nio.file.Path

import com.serenity.lsp.client.LspWatch.{FileChangeType, FileSystemWatcher, WatchedFileChange}

/** One server's live `workspace/didChangeWatchedFiles` registrations, by registration id (#2531). */
final case class LspWatchRegistrations(byId: Map[String, List[FileSystemWatcher]]):

  def register(id: String, watchers: List[FileSystemWatcher]): LspWatchRegistrations =
    LspWatchRegistrations(byId.updated(id, watchers))

  def unregister(id: String): LspWatchRegistrations = LspWatchRegistrations(byId - id)

  def isEmpty: Boolean = byId.values.forall(_.isEmpty)

  /** The events this server asked for, merged per path. `root` is the server's workspace root. */
  def select(changes: Vector[WatchedFileChange], root: Path): Vector[WatchedFileChange] =
    val watchers = byId.values.flatten.toVector
    LspWatchRegistrations.coalesce(changes.filter { change =>
      watchers.exists(watcher => watcher.kinds(change.change) && watcher.glob.matches(change.path, root))
    })

object LspWatchRegistrations:
  import FileChangeType.{Changed, Created, Deleted}

  val empty: LspWatchRegistrations = LspWatchRegistrations(Map.empty)

  /** One event per path, at the position of its first event, saying what the sequence amounts to: a file created and
    * then deleted is no event at all, and one deleted and then created again has changed.
    */
  def coalesce(changes: Vector[WatchedFileChange]): Vector[WatchedFileChange] =
    val net = changes.foldLeft(Map.empty[Path, Option[FileChangeType]]) { (seen, change) =>
      seen.updated(change.path, merged(seen.get(change.path).flatten, change.change))
    }
    changes.map(_.path).distinct.flatMap(path => net.get(path).flatten.map(WatchedFileChange(path, _)))

  private def merged(before: Option[FileChangeType], next: FileChangeType): Option[FileChangeType] =
    (before, next) match
      case (None, _)                => Some(next)
      case (Some(Created), Deleted) => None
      case (Some(Created), _)       => Some(Created)
      case (Some(Deleted), Deleted) => Some(Deleted)
      case (Some(Deleted), _)       => Some(Changed)
      case (Some(Changed), Deleted) => Some(Deleted)
      case (Some(Changed), _)       => Some(Changed)
