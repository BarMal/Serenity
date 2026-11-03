package com.serenity.io

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The parallel walk (#2414, #2522 item 3) must give exactly the serial walk's answer: the same files, the same cut at
  * the limit, the same partial listings, and the same handling of unreadable directories.
  */
class ProjectFileWalkerParallelSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 25)

  private val Past = FileTime.from(Instant.now.minusSeconds(3600))

  private def write(root: Path, relative: String, content: String = ""): Unit =
    val file = root.resolve(relative)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)

  private def aged(root: Path): Path =
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala.filter(Files.isDirectory(_)).foreach(Files.setLastModifiedTime(_, Past))
    }
    root

  private def wide(directories: Int, filesEach: Int): Path =
    val root = TestTemp.directory("parallel-walk")
    (0 until directories).foreach(d => (0 until filesEach).foreach(f => write(root, f"d$d%02d/f$f.txt")))
    aged(root)

  /** A random project: nested directories, ignored and hidden ones, and `.gitignore` files that include negations. */
  private val projectGen: Gen[(Vector[String], Map[String, String])] =
    val segment   = Gen.oneOf("a", "b", "c", "src", "node_modules", ".hidden", "target", "deep")
    val fileName  = Gen.oneOf("x.txt", "y.log", "z.scala", "w.class", "keep.log", "notes.md")
    val directory = Gen.choose(0, 4).flatMap(Gen.listOfN(_, segment)).map(_.mkString("/"))
    val file      = (directory, fileName).mapN((d, f) => if d.isEmpty then f else s"$d/$f")
    val pattern   = Gen.oneOf("*.log", "b/", "!keep.log", "/x.txt", "deep/", "*.md")
    val ignore = (directory, Gen.choose(1, 3).flatMap(Gen.listOfN(_, pattern))).mapN((d, ps) => d -> ps.mkString("\n"))
    (Gen.choose(1, 40).flatMap(Gen.listOfN(_, file)), Gen.choose(0, 3).flatMap(Gen.listOfN(_, ignore)))
      .mapN((files, ignores) => (files.toVector, ignores.toMap))

  extension [A, B](pair: (Gen[A], Gen[B]))
    private def mapN[C](f: (A, B) => C): Gen[C] = pair._1.flatMap(a => pair._2.map(f(a, _)))

  private def materialize(files: Vector[String], ignores: Map[String, String]): Path =
    val root = TestTemp.directory("parallel-walk-random")
    files.foreach(write(root, _))
    ignores.foreach((directory, rules) =>
      write(root, if directory.isEmpty then ".gitignore" else s"$directory/.gitignore", rules)
    )
    root

  /** A reader that holds each read it `holds` until a second such read has started, for at most five seconds. Reads
    * that never overlap each wait out the five seconds and leave `overlapped` false, so overlap is shown without a
    * wall-clock sleep.
    */
  final private class Rendezvous(reader: DirectoryReader, holds: Path => Boolean):
    private val bothIn   = new CountDownLatch(2)
    private val inFlight = new AtomicInteger(0)
    private val maximum  = new AtomicInteger(0)

    val read: DirectoryReader = directory =>
      if !holds(directory) then reader(directory)
      else
        val now = inFlight.incrementAndGet()
        val _   = maximum.accumulateAndGet(now, math.max)
        try
          bothIn.countDown()
          val _ = bothIn.await(5, TimeUnit.SECONDS)
          reader(directory)
        finally
          val _ = inFlight.decrementAndGet()

    def overlapped: Boolean = bothIn.getCount == 0 && maximum.get > 1
    def mostAtOnce: Int     = maximum.get

  "A parallel walk" should "list exactly what the serial walk lists, cut at the same place for every limit" in
    forAll(projectGen) { (files, ignores) =>
      val root   = materialize(files, ignores)
      val total  = ProjectFileWalker.list(root, limit = 100000, parallelism = 1).unsafeRunSync().files.size
      val limits = Vector(1, 2, 3, 7, total - 1, total, total + 1, 100000).filter(_ >= 1).distinct
      limits.foreach { limit =>
        ProjectFileWalker.list(root, limit, parallelism = 4).unsafeRunSync() shouldBe
          ProjectFileWalker.list(root, limit, parallelism = 1).unsafeRunSync()
      }
    }

  it should "report the same partial listings as the serial walk, each made of whole directories" in {
    val root = aged {
      val r = TestTemp.directory("parallel-walk-partials")
      (0 until 6).foreach(d => (0 until 3).foreach(f => write(r, s"d$d/sub/f$f.txt")))
      (0 until 6).foreach(d => write(r, s"d$d/top.txt"))
      r
    }

    def partials(parallelism: Int): (Vector[ProjectFileListing], ProjectFileListing) =
      val index = ProjectFileIndex.emptyReading(ProjectFileWalker.readDirectory, IO.pure(Vector.empty), 2, parallelism)
      (for
        seen     <- Ref.of[IO, Vector[ProjectFileListing]](Vector.empty)
        finished <- index.listing(root, partial => seen.update(_ :+ partial))
        partials <- seen.get
      yield (partials, finished)).unsafeRunSync()

    val (serial, serialFinal)     = partials(1)
    val (parallel, parallelFinal) = partials(4)

    parallel shouldBe serial
    parallelFinal shouldBe serialFinal
    val complete = parallelFinal.files.groupBy(file => Option(file.getParent)).view.mapValues(_.toSet).toMap
    parallel.foreach { partial =>
      partial.files
        .groupBy(file => Option(file.getParent))
        .foreach((parent, found) => found.toSet shouldBe complete(parent))
    }
  }

  it should "read several directories at once, but never more than its parallelism" in {
    val root   = wide(directories = 24, filesEach = 2)
    val meeter = Rendezvous(ProjectFileWalker.readDirectory, holds = _ != root)

    val walked = ProjectFileWalker
      .build(
        root,
        meeter.read,
        Vector(ProjectFileWalker.RootDirectory -> IgnoreRules.empty),
        Int.MaxValue,
        Int.MaxValue,
        _ => IO.unit,
        parallelism = 4
      )
      .unsafeRunSync()

    walked.files should have size 48
    meeter.overlapped shouldBe true
    meeter.mostAtOnce should be <= 4
  }

  it should "skip a directory that cannot be read, and still fail when the root cannot" in {
    val root = wide(directories = 6, filesEach = 1)
    val unreadable: DirectoryReader = directory =>
      if directory.getFileName.toString == "d03" then throw new java.io.IOException("denied")
      else ProjectFileWalker.readDirectory(directory)

    val walked = ProjectFileWalker
      .build(
        root,
        unreadable,
        Vector(ProjectFileWalker.RootDirectory -> IgnoreRules.empty),
        Int.MaxValue,
        Int.MaxValue,
        _ => IO.unit,
        parallelism = 4
      )
      .unsafeRunSync()
    walked.files.map(ProjectFileWalker.slashed).sorted shouldBe Vector(
      "d00/f0.txt",
      "d01/f0.txt",
      "d02/f0.txt",
      "d04/f0.txt",
      "d05/f0.txt"
    )

    ProjectFileWalker
      .list(root.resolve("missing"), limit = 10, parallelism = 4)
      .attempt
      .unsafeRunSync()
      .isLeft shouldBe true
  }

  it should "start no further reads once cancelled, beyond those already running" in {
    val root    = wide(directories = 8, filesEach = 1)
    val entered = new CountDownLatch(2)
    val release = new CountDownLatch(1)
    val started = new AtomicInteger(0)
    val reader: DirectoryReader = directory =>
      val _ = started.incrementAndGet()
      if directory != root then
        entered.countDown()
        val _ = release.await(10, TimeUnit.SECONDS)
      ProjectFileWalker.readDirectory(directory)

    val walk = ProjectFileWalker.build(
      root,
      reader,
      Vector(ProjectFileWalker.RootDirectory -> IgnoreRules.empty),
      Int.MaxValue,
      Int.MaxValue,
      _ => IO.unit,
      parallelism = 2
    )

    val both = (for
      fiber     <- walk.start
      reachedIt <- IO.interruptible(entered.await(5, TimeUnit.SECONDS))
      atCancel  <- IO(started.get)
      cancelled <- fiber.cancel.start
      _         <- IO(release.countDown())
      _         <- cancelled.joinWithNever
    yield (reachedIt, atCancel, started.get)).unsafeRunSync()

    val (reachedTwoReads, readsAtCancel, readsAfter) = both
    reachedTwoReads shouldBe true
    readsAfter shouldBe readsAtCancel
  }

  "Revalidating a project in parallel" should "read again exactly the directories that changed, several at once" in {
    val root    = wide(directories = 12, filesEach = 2)
    val seen    = new AtomicReference(Vector.empty[Path])
    val meeting = new AtomicReference(Option.empty[Rendezvous])
    val reader: DirectoryReader = directory =>
      val _ = seen.updateAndGet(_ :+ directory)
      meeting.get.fold(ProjectFileWalker.readDirectory(directory))(_.read(directory))
    val index = ProjectFileIndex.emptyReading(reader, IO.pure(Vector.empty), 4096, parallelism = 4)
    index.listing(root, _ => IO.unit).unsafeRunSync()

    val changed = Vector("d01", "d04", "d07", "d10").map(root.resolve(_))
    changed.foreach(directory => write(directory, "added.txt"))
    seen.set(Vector.empty)
    val meeter = Rendezvous(ProjectFileWalker.readDirectory, holds = _ => true)
    meeting.set(Some(meeter))

    val revalidated = index.listing(root, _ => IO.unit).unsafeRunSync()
    val fresh = ProjectFileIndex
      .emptyReading(ProjectFileWalker.readDirectory, IO.pure(Vector.empty), 4096, parallelism = 1)
      .listing(root, _ => IO.unit)
      .unsafeRunSync()

    revalidated shouldBe fresh
    seen.get.toSet shouldBe changed.toSet
    meeter.overlapped shouldBe true
  }
