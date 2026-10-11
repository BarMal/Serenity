package com.serenity.perf

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{Files, LinkOption, Path}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.ProjectFileIndex

/** Before/after for #1892 over a 50,000-file synthetic tree, in one JVM so both sides share a warm-up. "Before" is the
  * walker as it was on master (copied below, 20,000-file cap lifted so it does the same work), which every finder open
  * ran; "after" is the first open of the project index (a full walk, now also reading the ignore files) and each later
  * open (a check of the directories' modification times). Run with
  * `Test / runMain com.serenity.perf.ProjectWalkBenchmark`.
  */
object ProjectWalkBenchmark:

  private val Directories    = 500
  private val FilesPerFolder = 100

  private def millis(work: => Unit): Double =
    val start = System.nanoTime()
    work
    (System.nanoTime() - start) / 1e6

  private def synthetic(): Path =
    val root = TestTemp.directory("project-walk-bench")
    (0 until Directories).foreach { directory =>
      val path = Files.createDirectories(root.resolve(s"module${directory % 20}/pkg$directory"))
      (0 until FilesPerFolder).foreach(file => Files.createFile(path.resolve(s"File$file.scala")))
    }
    Files.writeString(root.resolve(".gitignore"), "*.log\nout/\n")
    root

  def main(args: Array[String]): Unit =
    val root = synthetic()
    println(s"tree: ${Directories * FilesPerFolder} files in $Directories directories under $root")
    (1 to 3).foreach { round =>
      val before = millis(MasterWalker.list(root, Int.MaxValue - 1).unsafeRunSync().size.ensuring(_ > 0))
      val index  = ProjectFileIndex.empty()
      val first  = millis(index.listing(root, _ => IO.unit).unsafeRunSync().files.size.ensuring(_ > 0))
      Thread.sleep(2500)
      val reopen = millis(index.listing(root, _ => IO.unit).unsafeRunSync().files.size.ensuring(_ > 0))
      println(
        f"round $round: master open (full walk) $before%.1f ms | index first open $first%.1f ms | index reopen $reopen%.1f ms"
      )
    }

  /** `ProjectFileWalker` as at master 4f74549, unchanged apart from its name and the return type. */
  private object MasterWalker:
    private val SkippedDirectories: Set[String] = Set("target", "node_modules", "dist", "build")

    def list(root: Path, limit: Int): IO[Vector[Path]] =
      IO.blocking {
        val (rootFiles, rootDirectories) = entries(root)
        val (files, _)                   = walk(rootDirectories, rootFiles.take(limit + 1), limit)
        files.take(limit).map(root.relativize).sortBy(_.toString)
      }

    @tailrec
    private def walk(pending: Vector[Path], found: Vector[Path], limit: Int): (Vector[Path], Boolean) =
      if found.size > limit then (found, true)
      else
        pending match
          case directory +: rest =>
            val (files, directories) =
              try entries(directory)
              catch case _: IOException | _: UncheckedIOException => (Vector.empty, Vector.empty)
            walk(rest ++ directories, found ++ files.take(limit + 1 - found.size), limit)
          case _ => (found, false)

    private def entries(directory: Path): (Vector[Path], Vector[Path]) =
      val children              = Using.resource(Files.list(directory))(_.iterator.asScala.toVector.sortBy(_.toString))
      val (directories, others) = children.partition(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
      (others.filter(Files.isRegularFile(_)), directories.filter(walked))

    private def walked(directory: Path): Boolean =
      Option(directory.getFileName).map(_.toString).exists(name => !name.startsWith(".") && !SkippedDirectories(name))
