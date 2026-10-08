package com.serenity.io

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppKitOpenPanelSpec extends AnyFlatSpec with Matchers:

  private def probeNotToBeRun(): Boolean = fail("probed a platform that is not macOS")

  "The AppKit open panel" should "never be probed off macOS" in {
    AppKitOpenPanel.isAvailable("Linux", () => probeNotToBeRun()) shouldBe false
    AppKitOpenPanel.isAvailable("Windows 11", () => probeNotToBeRun()) shouldBe false
  }

  it should "be available on macOS when the Objective-C runtime answers" in {
    AppKitOpenPanel.isAvailable("Mac OS X", () => true) shouldBe true
    AppKitOpenPanel.isAvailable("Mac OS X", () => false) shouldBe false
  }

  it should "be unavailable on macOS when the runtime cannot be loaded" in {
    AppKitOpenPanel.isAvailable("Mac OS X", () => throw new UnsatisfiedLinkError("libobjc")) shouldBe false
    AppKitOpenPanel.isAvailable("Mac OS X", () => throw new IllegalStateException("no AppKit")) shouldBe false
  }

  private val chosen = Path.of("/Users/ada/notes")

  private def chooseWith(
    onMainThread: AppKitOpenPanel.OnMainThread,
    script: Option[Path] => Either[String, Option[Path]]
  ): IO[Option[Path]] =
    AppKitOpenPanel.chooseWith(onMainThread, script)(None)

  private val inline: AppKitOpenPanel.OnMainThread = new AppKitOpenPanel.OnMainThread:
    def apply[A](body: => A): Either[Throwable, A] = Right(body)

  "Choosing" should "return what the panel returned, or None for a cancel" in {
    chooseWith(inline, _ => Right(Some(chosen))).unsafeRunSync() shouldBe Some(chosen)
    chooseWith(inline, _ => Right(None)).unsafeRunSync() shouldBe None
  }

  it should "pass the starting directory to the panel" in {
    val seen = AppKitOpenPanel.chooseWith(inline, initial => Right(initial))(Some(chosen))

    seen.unsafeRunSync() shouldBe Some(chosen)
  }

  it should "fail the IO with the panel's own message when the script fails" in {
    val failure = chooseWith(inline, _ => Left("NSOpenPanel answered OK without a URL")).attempt.unsafeRunSync()

    failure.left.map(_.getMessage) shouldBe Left("NSOpenPanel answered OK without a URL")
  }

  it should "fail the IO when the main thread cannot be reached" in {
    val unreachable = new AppKitOpenPanel.OnMainThread:
      def apply[A](body: => A): Either[Throwable, A] = Left(new IllegalStateException("main thread did not respond"))

    val failure = chooseWith(unreachable, _ => Right(Some(chosen))).attempt.unsafeRunSync()

    failure.left.map(_.getMessage) shouldBe Left("main thread did not respond")
  }
