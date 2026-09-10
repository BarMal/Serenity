package com.serenity.io

import java.nio.file.{Files, Path}
import scala.compiletime.uninitialized

import cats.effect.{IO, Ref}
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #1416: `listDirectory` must not serialize its per-entry blocking IO one file at a time. */
class FileBrowserSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private var tempDir: Path = uninitialized

  override def beforeAll(): Unit =
    tempDir = Files.createTempDirectory("file-browser-spec")

  override def afterAll(): Unit =
    def delete(path: Path): Unit =
      if Files.isDirectory(path) then
        import scala.jdk.CollectionConverters.*
        Files.list(path).iterator().asScala.toList.foreach(delete)
      Files.deleteIfExists(path)
    delete(tempDir)

  "listDirectory" should "list a large directory's files and subdirectories, sorted directories-first then alphabetically" in {
    val fileCount = 300
    val dirCount  = 20

    (0 until fileCount).foreach(index => Files.writeString(tempDir.resolve(f"file-$index%04d.txt"), "x"))
    (0 until dirCount).foreach(index => Files.createDirectory(tempDir.resolve(f"dir-$index%04d")))

    val start   = System.nanoTime()
    val entries = FileBrowser.listDirectory(tempDir).unsafeRunSync()
    val elapsedMillis = (System.nanoTime() - start) / 1000000L
    info(s"listDirectory over ${fileCount + dirCount} entries took ${elapsedMillis}ms")

    entries should have size (fileCount + dirCount).toLong
    entries.count(_.isDirectory) shouldBe dirCount
    entries.count(!_.isDirectory) shouldBe fileCount

    // Directories first, then alphabetical by lowercased name within each group.
    val (dirEntries, fileEntries) = entries.splitAt(dirCount)
    dirEntries.forall(_.isDirectory) shouldBe true
    fileEntries.forall(!_.isDirectory) shouldBe true
    dirEntries.map(_.name) shouldBe dirEntries.map(_.name).sortBy(_.toLowerCase)
    fileEntries.map(_.name) shouldBe fileEntries.map(_.name).sortBy(_.toLowerCase)
    fileEntries.foreach(_.size shouldBe 1L)
  }

  it should "return an empty list for an empty directory" in {
    val emptyDir = Files.createTempDirectory("file-browser-spec-empty")
    try FileBrowser.listDirectory(emptyDir).unsafeRunSync() shouldBe empty
    finally Files.deleteIfExists(emptyDir)
  }

  "the parTraverseN concurrency bound FileBrowser relies on" should
    "run entries concurrently rather than serializing them" in {
      val itemCount    = FileBrowser.MaxConcurrentEntries * 4
      val boundedCount = FileBrowser.MaxConcurrentEntries

      def probe(concurrentRef: Ref[IO, Int], maxRef: Ref[IO, Int]): IO[Unit] =
        for
          current <- concurrentRef.updateAndGet(_ + 1)
          _       <- maxRef.update(_ max current)
          _       <- IO.sleep(scala.concurrent.duration.DurationInt(20).millis)
          _       <- concurrentRef.update(_ - 1)
        yield ()

      val result = (for
        concurrentRef <- Ref.of[IO, Int](0)
        maxRef        <- Ref.of[IO, Int](0)
        _             <- List.fill(itemCount)(()).parTraverseN(boundedCount)(_ => probe(concurrentRef, maxRef))
        max           <- maxRef.get
      yield max).unsafeRunSync()

      // Genuinely parallel (more than one in flight at once) but never exceeding the configured bound.
      result should be > 1
      result should be <= boundedCount
    }

end FileBrowserSpec
