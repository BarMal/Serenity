package com.serenity.input

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExternalToolClipboardSpec extends AnyFlatSpec with Matchers:

  final private case class StubRunner(
      calls: Ref[IO, List[(List[String], Option[String])]],
      responses: Map[List[String], String] = Map.empty,
      failing: Set[List[String]] = Set.empty
  ) extends ExternalToolClipboard.ProcessRunner:

    override def run(command: List[String], stdin: Option[String]): IO[String] =
      calls.update(_ :+ (command -> stdin)) >>
        (if failing.contains(command) then IO.raiseError(new RuntimeException(s"$command failed"))
         else IO.pure(responses.getOrElse(command, "")))

  "ExternalToolClipboard.writeText" should "pipe the text as stdin to the tool's write command" in {
    val calls     = Ref.unsafe[IO, List[(List[String], Option[String])]](Nil)
    val runner    = StubRunner(calls)
    val clipboard = ExternalToolClipboard(ExternalClipboardTool.Xclip, runner)

    clipboard.writeText("copied text").unsafeRunSync()

    calls.get.unsafeRunSync() shouldBe List(
      ExternalClipboardTool.Xclip.writeCommand -> Some("copied text")
    )
  }

  "ExternalToolClipboard.readText" should "return the tool's read-command stdout" in {
    val calls = Ref.unsafe[IO, List[(List[String], Option[String])]](Nil)
    val runner = StubRunner(
      calls,
      responses = Map(ExternalClipboardTool.Xsel.readCommand -> "pasted text")
    )
    val clipboard = ExternalToolClipboard(ExternalClipboardTool.Xsel, runner)

    clipboard.readText.unsafeRunSync() shouldBe Some("pasted text")
    calls.get.unsafeRunSync() shouldBe List(ExternalClipboardTool.Xsel.readCommand -> None)
  }

  it should "report no text rather than raising when the tool fails" in {
    val calls     = Ref.unsafe[IO, List[(List[String], Option[String])]](Nil)
    val runner    = StubRunner(calls, failing = Set(ExternalClipboardTool.WlClipboard.readCommand))
    val clipboard = ExternalToolClipboard(ExternalClipboardTool.WlClipboard, runner)

    clipboard.readText.unsafeRunSync() shouldBe None
  }

  "ExternalToolClipboard.writeText" should "not raise when the tool fails" in {
    val calls     = Ref.unsafe[IO, List[(List[String], Option[String])]](Nil)
    val runner    = StubRunner(calls, failing = Set(ExternalClipboardTool.WlClipboard.writeCommand))
    val clipboard = ExternalToolClipboard(ExternalClipboardTool.WlClipboard, runner)

    clipboard.writeText("text").unsafeRunSync()
  }
