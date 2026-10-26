package com.serenity.app

import java.nio.file.Paths

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2388: dropped paths reach the editor's forwarded-open loop without the AWT thread doing any work. */
class FileDropsSpec extends AnyFlatSpec with Matchers:

  private val first  = Paths.get("/work/first.md")
  private val second = Paths.get("/work/second.md")

  "FileDrops" should "deliver each dropped batch, in order, on its stream" in {
    val program = FileDrops.create(_ => IO.unit).use { drops =>
      IO(drops.dropped(List(first))) >> IO(drops.dropped(List(second, first))) >>
        drops.opened.take(2).compile.toList.timeout(5.seconds)
    }

    program.unsafeRunSync() shouldBe List(List(first), List(second, first))
  }

  it should "send the description of what a drag offers to the log" in {
    val program =
      for
        logged <- Deferred[IO, String]
        line <- FileDrops.create(logged.complete(_).void).use { drops =>
          IO(drops.offered("application/x-java-file-list")) >> logged.get.timeout(5.seconds)
        }
      yield line

    program.unsafeRunSync() shouldBe "application/x-java-file-list"
  }
