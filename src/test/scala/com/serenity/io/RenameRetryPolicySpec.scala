package com.serenity.io

import java.io.IOException
import java.nio.file.{
  AccessDeniedException,
  AtomicMoveNotSupportedException,
  DirectoryNotEmptyException,
  FileSystemException,
  Files,
  NoSuchFileException,
  Path
}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The retry around the atomic rename (#2016): on Windows an antivirus scanner or the search indexer can hold the file
  * for a moment, and the rename then fails with `AccessDeniedException`.
  */
class RenameRetryPolicySpec extends AnyFlatSpec with Matchers:

  /** The real filesystem, except that the first `failures` renames raise `failure`. */
  final private class FlakyRename(failures: Int, failure: Path => Throwable) extends AtomicFileSystem:
    private val real = AtomicFileWriter.defaultFileSystem
    val renames      = AtomicInteger(0)

    export real.{moveAtomically as _, *}

    override def moveAtomically(source: Path, target: Path): Path =
      if renames.incrementAndGet() <= failures then throw failure(target)
      else real.moveAtomically(source, target)

  private val quick = RenameRetryPolicy.exponential(attempts = 5, initial = 1.millis)

  private def written(failures: Int, failure: Path => Throwable, policy: RenameRetryPolicy = quick) =
    val directory = Files.createTempDirectory("rename-retry-spec")
    val target    = directory.resolve("document.txt")
    Files.writeString(target, "before")
    val fileSystem = FlakyRename(failures, failure)
    val outcome    = AtomicFileWriter.writeBytes(target, "after".getBytes, fileSystem, policy).attempt.unsafeRunSync()
    (outcome, fileSystem.renames.get, target)

  private def denied(target: Path): Throwable = AccessDeniedException(target.toString)

  "The default policy" should "make five attempts over about half a second" in {
    RenameRetryPolicy.default.attempts shouldBe 5
    RenameRetryPolicy.default.delays shouldBe List(35.millis, 70.millis, 140.millis, 280.millis)
    RenameRetryPolicy.default.delays.reduce(_ + _) should (be >= 400.millis and be <= 600.millis)
  }

  "exponential" should "double each pause and allow one attempt more than it has pauses" in {
    RenameRetryPolicy.exponential(attempts = 4, initial = 10.millis).delays shouldBe
      List(10.millis, 20.millis, 40.millis)
    RenameRetryPolicy.exponential(attempts = 1, initial = 10.millis).delays shouldBe Nil
  }

  "retryable" should "cover access denial and sharing violations but not errors that waiting cannot clear" in {
    val policy = RenameRetryPolicy.default
    policy.retryable(AccessDeniedException("x")) shouldBe true
    policy.retryable(FileSystemException("x")) shouldBe true
    policy.retryable(NoSuchFileException("x")) shouldBe false
    policy.retryable(DirectoryNotEmptyException("x")) shouldBe false
    policy.retryable(AtomicMoveNotSupportedException("a", "b", "unsupported")) shouldBe false
    policy.retryable(IOException("disk full")) shouldBe false
  }

  "A write whose rename is refused a few times" should "land once the hold passes" in {
    val (outcome, renames, target) = written(failures = 3, denied)

    outcome shouldBe Right(())
    renames shouldBe 4
    Files.readString(target) shouldBe "after"
    Files.list(target.getParent).toArray should have size 1
  }

  it should "retry a sharing-violation FileSystemException too" in {
    val (outcome, renames, target) =
      written(failures = 2, path => FileSystemException(path.toString))

    outcome shouldBe Right(())
    renames shouldBe 3
    Files.readString(target) shouldBe "after"
  }

  "A write whose rename keeps being refused" should "fail with the access error after the last attempt, leaving the file as it was" in {
    val (outcome, renames, target) = written(failures = Int.MaxValue, denied)

    renames shouldBe quick.attempts
    outcome.left.toOption.map(_.getClass) shouldBe Some(classOf[AtomicFileWriteException])
    outcome.left.toOption.flatMap(error => Option(error.getCause)).map(_.getClass) shouldBe
      Some(classOf[AccessDeniedException])
    Files.readString(target) shouldBe "before"
    Files.list(target.getParent).toArray should have size 1
  }

  "A write that fails for a reason waiting cannot clear" should "not be retried" in {
    val (outcome, renames, _) = written(failures = Int.MaxValue, path => NoSuchFileException(path.toString))

    renames shouldBe 1
    outcome.isLeft shouldBe true
  }

  "A write with no retry policy" should "fail on the first refusal" in {
    val (outcome, renames, _) = written(failures = 1, denied, RenameRetryPolicy.none)

    renames shouldBe 1
    outcome.isLeft shouldBe true
  }
