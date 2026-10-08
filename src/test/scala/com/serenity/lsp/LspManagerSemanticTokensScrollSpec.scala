package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{SemanticTokenData, SemanticTokensFeatures}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec

/** Scrolling while semantic tokens are pending or stale: the lines scrolled into view are requested on their own,
  * debounced, folded into a refresh already waiting, and never requested twice at once (#1837).
  */
class LspManagerSemanticTokensScrollSpec extends AnyFlatSpec with SemanticTokensFakeServer:

  private val Debounce = LspManagerSemanticTokens.DefaultDebounce

  private val fullMethod  = "textDocument/semanticTokens/full"
  private val rangeMethod = "textDocument/semanticTokens/range"

  private val threeTokens = List(0, 0, 3, 0, 0, 1, 0, 4, 1, 0, 1, 0, 5, 2, 0)

  private def decoded(data: List[Int]): SemanticTokenData =
    SemanticTokenData.decode(IArray.from(data), legend)

  private def received(data: List[Int]): LspEvent =
    LspEvent.LspSemanticTokensReceived(uri, decoded(data))

  private def positionOf(params: io.circe.Json, edge: String): Option[(Int, Int)] =
    val position = params.hcursor.downField("range").downField(edge)
    for
      line      <- position.downField("line").as[Int].toOption
      character <- position.downField("character").as[Int].toOption
    yield (line, character)

  private val everything = SemanticTokensFeatures(full = true, delta = true, range = true)

  "LspManager" should "ask for the lines scrolled into view while a large document's full answer is pending, and use them" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(everything)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _       <- server.open("x\n" * 6000)
          opening <- List.fill(2)(server.nextRequest).sequence
          full = opening.find(_.method == fullMethod)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 100, 130))
          _        <- server.assertQuietFor(Debounce - 50.millis)
          scrolled <- server.nextRequest
          _ = scrolled.method shouldBe rangeMethod
          _ = positionOf(scrolled.params, "start") shouldBe Some((100, 0))
          _ = positionOf(scrolled.params, "end") shouldBe Some((131, 0))
          _    <- server.replyWithTokens(scrolled, List(110, 0, 3, 0, 0))
          seen <- server.awaitEvents(1)
          _ = seen shouldBe List(LspEvent.LspSemanticTokensRangeReceived(uri, 100, 130, decoded(List(110, 0, 3, 0, 0))))
          _     <- full.traverse_(server.replyWithTokens(_, List(0, 0, 3, 0, 0), resultId = Some("1")))
          after <- server.awaitEvents(2)
          _ = after.lastOption shouldBe Some(received(List(0, 0, 3, 0, 0)))
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask once for lines scrolled to repeatedly while that request is in flight" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(everything)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _        <- server.open("x\n" * 6000)
          _        <- List.fill(2)(server.nextRequest).sequence
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 100, 130))
          scrolled <- server.nextRequest
          _ = scrolled.method shouldBe rangeMethod
          _ <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 100, 130))
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "cancel the request for lines the viewport has left when it asks for others" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(everything)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _       <- server.open("x\n" * 6000)
          opening <- List.fill(2)(server.nextRequest).sequence
          earlier = opening.find(_.method == rangeMethod)
          _      <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 100, 130))
          cancel <- server.nextMessage
          _ = server.methodOf(cancel) shouldBe "$/cancelRequest"
          _ = cancel.hcursor.downField("params").downField("id").as[Long].toOption shouldBe earlier.map(_.id)
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask for nothing after a scroll once the whole document's answer is shown for its text" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(fullWithRange)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _       <- server.open()
          opening <- List.fill(2)(server.nextRequest).sequence
          _       <- opening.find(_.method == fullMethod).traverse_(server.replyWithTokens(_, threeTokens))
          _       <- server.awaitEvents(1)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 50, 80))
          _       <- server.assertQuietFor(2.seconds)
          _       <- server.stop
        yield succeed
      }
    )

  it should "fold a scroll into the refresh an edit is already waiting to make" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(fullWithRange)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _       <- server.open()
          opening <- List.fill(2)(server.nextRequest).sequence
          _       <- opening.find(_.method == fullMethod).traverse_(server.replyWithTokens(_, threeTokens))
          _       <- server.awaitEvents(1)
          _       <- opening.find(_.method == rangeMethod).traverse_(server.replyWithTokens(_, Nil))
          _       <- IO.sleep(20.millis)
          _       <- server.edit("object Foo1", 2)
          _       <- server.nextMessage
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 50, 80))
          _       <- server.assertQuietFor(Debounce - 50.millis)
          refresh <- List.fill(2)(server.nextRequest).sequence
          _     = refresh.map(_.method).toSet shouldBe Set(fullMethod, rangeMethod)
          range = refresh.find(_.method == rangeMethod)
          _     = range.flatMap(request => positionOf(request.params, "start")) shouldBe Some((50, 0))
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask for the lines scrolled into view after an edit while the document's new answer is pending" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(fullWithRange)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _        <- server.open()
          opening  <- List.fill(2)(server.nextRequest).sequence
          _        <- opening.find(_.method == fullMethod).traverse_(server.replyWithTokens(_, threeTokens))
          _        <- server.awaitEvents(1)
          _        <- opening.find(_.method == rangeMethod).traverse_(server.replyWithTokens(_, Nil))
          _        <- IO.sleep(20.millis)
          _        <- server.edit("object Foo1", 2)
          _        <- server.nextMessage
          refresh  <- List.fill(2)(server.nextRequest).sequence
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 50, 80))
          scrolled <- server.nextRequest
          _ = scrolled.method shouldBe rangeMethod
          _ = positionOf(scrolled.params, "start") shouldBe Some((50, 0))
          _ = refresh.map(_.method).toSet shouldBe Set(fullMethod, rangeMethod)
          _ <- server.stop
        yield succeed
      }
    )
