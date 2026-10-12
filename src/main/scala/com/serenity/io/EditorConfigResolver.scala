package com.serenity.io

import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.util.Try

import cats.effect.IO
import com.serenity.state.models.IndentationHint

/** Finds the `.editorconfig` files that speak for a path: those in its directory and every directory above it, up to
  * the first that says `root = true`. Each directory's file is parsed once and kept; every lookup stats it again, so an
  * edited file is read afresh the next time a document is opened or reloaded, with no watcher involved.
  */
final class EditorConfigResolver:

  final private case class Cached(stamp: (FileTime, Long), file: EditorConfigFile)

  private val cache = AtomicReference(Map.empty[Path, Cached])

  /** What the files say about `path`'s indentation. A file that cannot be read says nothing. */
  def indentationFor(path: Path): IO[IndentationHint] =
    IO.blocking {
      val absolute = path.toAbsolutePath.normalize
      EditorConfig.indentation(EditorConfig.propertiesFor(layersFor(absolute), absolute))
    }.handleError(_ => IndentationHint.Empty)

  // Farthest directory first, so that the nearer file is applied last.
  private def layersFor(path: Path): List[EditorConfig.Layer] =
    @tailrec def climb(directory: Option[Path], layers: List[EditorConfig.Layer]): List[EditorConfig.Layer] =
      directory match
        case None => layers
        case Some(current) =>
          val found = fileIn(current)
          val next  = found.fold(layers)(EditorConfig.Layer(current, _) :: layers)
          if found.exists(_.root) then next else climb(Option(current.getParent), next)
    climb(Option(path.getParent), Nil)

  private def fileIn(directory: Path): Option[EditorConfigFile] =
    val location = directory.resolve(EditorConfig.FileName)
    Try(Files.readAttributes(location, classOf[BasicFileAttributes])).toOption.filter(_.isRegularFile) match
      case None =>
        val _ = cache.updateAndGet(_ - directory)
        None
      case Some(attributes) =>
        val stamp = (attributes.lastModifiedTime, attributes.size)
        cache.get.get(directory).filter(_.stamp == stamp) match
          case Some(cached) => Some(cached.file)
          case None =>
            Try(Files.readString(location)).toOption.map { text =>
              val file = EditorConfig.parse(text)
              val _    = cache.updateAndGet(_.updated(directory, Cached(stamp, file)))
              file
            }
