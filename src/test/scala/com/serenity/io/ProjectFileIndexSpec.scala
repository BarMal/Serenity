package com.serenity.io

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectFileIndexSpec extends AnyFlatSpec with Matchers:

  private class Reads:
    private val seen = new AtomicReference(Vector.empty[Path])

    val reader: DirectoryReader = directory =>
      val _ = seen.updateAndGet(_ :+ directory)
      ProjectFileWalker.readDirectory(directory)

    def directories: Vector[Path] = seen.get
    def clear(): Unit             = seen.set(Vector.empty)

  private def write(root: Path, relative: String, content: String = ""): Unit =
    val file = root.resolve(relative)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)

  private def tree(paths: String*): Path =
    val root = TestTemp.directory("project-file-index")
    paths.foreach(write(root, _))
    root

  private val Past = FileTime.from(Instant.now.minusSeconds(3600))

  /** Backdates every directory, so none is mistaken for one that changed in the same clock tick it was read in. */
  private def aged(root: Path): Path =
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala.filter(Files.isDirectory(_)).foreach(Files.setLastModifiedTime(_, Past))
    }
    root

  private def later(file: Path): Unit =
    val _ = Files.setLastModifiedTime(file, FileTime.from(Instant.now.plusSeconds(60)))

  private def index(reads: Reads, global: IO[Vector[Path]] = IO.pure(Vector.empty)): ProjectFileIndex =
    ProjectFileIndex.emptyReading(reads.reader, global, firstPartialAt = 4096)

  private def listed(index: ProjectFileIndex, root: Path): Vector[String] =
    index.listing(root, _ => IO.unit).unsafeRunSync().files.map(_.toString)

  "ProjectFileIndex" should "list a project's files without those its ignore files exclude" in {
    val root = tree("a.txt", "src/b.log", "src/c.txt", "out/d.txt")
    write(root, ".gitignore", "*.log\nout/\n")

    listed(index(new Reads), root) shouldBe Vector(".gitignore", "a.txt", "src/c.txt")
  }

  it should "reuse what it found when a project is listed again, reading no directory" in {
    val root    = aged(tree("a.txt", "src/b.txt", "src/deep/c.txt"))
    val reads   = new Reads
    val subject = index(reads)

    val first = listed(subject, root)
    reads.directories should have size 3
    reads.clear()

    listed(subject, root) shouldBe first
    reads.directories shouldBe empty
  }

  it should "read again only the directory a file was added to or removed from" in {
    val root    = aged(tree("a.txt", "src/b.txt", "docs/c.txt"))
    val reads   = new Reads
    val subject = index(reads)
    listed(subject, root)

    reads.clear()
    write(root, "src/new.txt")
    listed(subject, root) shouldBe Vector("a.txt", "docs/c.txt", "src/b.txt", "src/new.txt")
    reads.directories shouldBe Vector(root.resolve("src"))

    reads.clear()
    Files.delete(root.resolve("src/b.txt"))
    listed(subject, root) shouldBe Vector("a.txt", "docs/c.txt", "src/new.txt")
    reads.directories shouldBe Vector(root.resolve("src"))
  }

  it should "walk a directory that appeared and forget one that disappeared" in {
    val root    = aged(tree("a.txt", "old/x.txt", "keep/y.txt"))
    val reads   = new Reads
    val subject = index(reads)
    listed(subject, root)

    reads.clear()
    write(root, "fresh/inner/z.txt")
    DirectoryTree.deleteBlocking(root.resolve("old"))
    listed(subject, root) shouldBe Vector("a.txt", "fresh/inner/z.txt", "keep/y.txt")
    reads.directories.toSet shouldBe Set(root, root.resolve("fresh"), root.resolve("fresh/inner"))
  }

  it should "see a file added right after a listing, before the filesystem clock has moved on" in {
    val root    = tree("a.txt", "src/b.txt")
    val subject = index(new Reads)
    listed(subject, root)

    write(root, "src/c.txt")

    listed(subject, root) shouldBe Vector("a.txt", "src/b.txt", "src/c.txt")
  }

  it should "re-walk a directory's subtree when its .gitignore is edited" in {
    val root = tree("pkg/a.log", "pkg/inner/b.log", "pkg/c.txt")
    write(root, "pkg/.gitignore", "*.log\n")
    aged(root)
    val subject = index(new Reads)
    listed(subject, root) shouldBe Vector("pkg/.gitignore", "pkg/c.txt")

    Files.writeString(root.resolve("pkg/.gitignore"), "c.txt\n")
    later(root.resolve("pkg/.gitignore"))

    listed(subject, root) shouldBe Vector("pkg/.gitignore", "pkg/a.log", "pkg/inner/b.log")
  }

  it should "pick up a .gitignore created after the project was listed" in {
    val root    = aged(tree("a.log", "b.txt"))
    val subject = index(new Reads)
    listed(subject, root) shouldBe Vector("a.log", "b.txt")

    write(root, ".gitignore", "*.log\n")

    listed(subject, root) shouldBe Vector(".gitignore", "b.txt")
  }

  it should "apply a global excludes file, and notice when it changes" in {
    val root   = aged(tree("a.tmp", "b.txt"))
    val global = TestTemp.directory("global-excludes").resolve("ignore")
    Files.writeString(global, "*.tmp\n")
    val subject = index(new Reads, IO.pure(Vector(global)))
    listed(subject, root) shouldBe Vector("b.txt")

    Files.writeString(global, "\n")
    later(global)

    listed(subject, root) shouldBe Vector("a.tmp", "b.txt")
  }

  it should "list every file of a project with more than twenty thousand" in {
    val root = TestTemp.directory("project-file-index-large")
    (0 until 21).foreach { directory =>
      val path = Files.createDirectories(root.resolve(s"dir$directory"))
      (0 until 1000).foreach(file => Files.createFile(path.resolve(s"file$file.txt")))
    }

    val listing = index(new Reads).listing(root, _ => IO.unit).unsafeRunSync()

    listing.files should have size 21000
    listing.truncated shouldBe false
    listing.files should contain(Path.of("dir20/file999.txt"))
  }

  it should "report what it has found so far while the first walk is still running" in {
    val root = aged(tree("a/1.txt", "a/2.txt", "b/3.txt", "b/4.txt", "c/5.txt", "c/6.txt"))
    val subject =
      ProjectFileIndex.emptyReading(ProjectFileWalker.readDirectory, IO.pure(Vector.empty), firstPartialAt = 2)

    val (partials, finished) = (for
      seen     <- Ref.of[IO, Vector[ProjectFileListing]](Vector.empty)
      finished <- subject.listing(root, partial => seen.update(_ :+ partial))
      partials <- seen.get
    yield (partials, finished)).unsafeRunSync()

    partials should not be empty
    all(partials.map(_.truncated)) shouldBe true
    partials.map(_.files.size) shouldBe partials.map(_.files.size).sorted
    partials.map(_.files.size).lastOption.exists(_ < finished.files.size) shouldBe true
    finished.files should have size 6
    finished.truncated shouldBe false
  }

  it should "fail, and forget the project, when its root can no longer be read" in {
    val root    = aged(tree("a.txt"))
    val subject = index(new Reads)
    listed(subject, root)

    DirectoryTree.deleteBlocking(root)

    subject.listing(root, _ => IO.unit).attempt.unsafeRunSync().isLeft shouldBe true
  }
