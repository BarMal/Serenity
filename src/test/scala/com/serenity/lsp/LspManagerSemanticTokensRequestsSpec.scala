package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{LspPosition, LspRange, SemanticTokenData, SemanticTokensFeatures, TextChangeDiff}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec

/** The semantic tokens requests `LspManager` sends -- debounced, as `full/delta` or `range` where the server offers
  * them, never answered with a response for an older document -- against a fake server on a virtual clock (#1837).
  */
class LspManagerSemanticTokensRequestsSpec extends AnyFlatSpec with SemanticTokensFakeServer:

  private val Debounce = LspManagerSemanticTokens.DefaultDebounce

  private val fullOnly = SemanticTokensFeatures.FullOnly

  private val fullMethod  = "textDocument/semanticTokens/full"
  private val deltaMethod = "textDocument/semanticTokens/full/delta"
  private val rangeMethod = "textDocument/semanticTokens/range"

  // keyword at 0:0..3, string at 1:0..4, comment at 2:0..5
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

  "LspManager" should "send one request once a burst of edits goes quiet for the debounce delay" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.open()
          _       <- server.announce(fullOnly)
          _       <- server.edit("object Foo1", 2)
          _       <- IO.sleep(50.millis)
          _       <- server.edit("object Foo12", 3)
          _       <- IO.sleep(50.millis)
          _       <- server.edit("object Foo123", 4)
          changes <- List.fill(3)(server.nextMessage).sequence
          _ = changes.map(server.methodOf) shouldBe List.fill(3)("textDocument/didChange")
          lastEditAt <- IO.monotonic
          _          <- server.assertQuietFor(Debounce - 50.millis)
          request    <- server.nextRequest
          requestAt  <- IO.monotonic
          _ = request.method shouldBe fullMethod
          _ = (requestAt - lastEditAt) should be >= Debounce
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "request tokens straight away when a document is opened" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(fullOnly)
          _       <- server.open()
          opened  <- IO.monotonic
          request <- server.nextRequest
          sentAt  <- IO.monotonic
          _ = request.method shouldBe fullMethod
          _ = (sentAt - opened) should be < Debounce
          _ <- server.stop
        yield succeed
      }
    )

  it should "cancel the request in flight when a newer edit arrives and ignore its late answer" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.open()
          _        <- server.announce(fullOnly)
          _        <- server.edit("object Foo1", 2)
          _        <- server.nextMessage
          inFlight <- server.nextRequest
          _        <- server.edit("object Foo12", 3)
          cancel   <- server.nextMessage
          _ = server.methodOf(cancel) shouldBe "$/cancelRequest"
          _ = cancel.hcursor.downField("params").downField("id").as[Long].toOption shouldBe Some(inFlight.id)
          _        <- server.nextMessage
          _        <- server.replyWithTokens(inFlight, threeTokens)
          _        <- IO.sleep(20.millis)
          lateSeen <- server.events.get
          _ = lateSeen shouldBe Nil
          current <- server.nextRequest
          _       <- server.replyWithTokens(current, List(0, 0, 3, 0, 0))
          seen    <- server.awaitEvents(1)
          _ = seen shouldBe List(received(List(0, 0, 3, 0, 0)))
          _ <- server.stop
        yield succeed
      }
    )

  it should "drop the answer to a request made for a document version that has since changed" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.open()
          _       <- server.announce(fullOnly)
          _       <- server.send(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala))
          request <- server.nextRequest
          _       <- server.edit("object Foo1", 2)
          _       <- server.nextMessage
          _       <- server.nextMessage
          _       <- server.replyWithTokens(request, threeTokens)
          _       <- IO.sleep(20.millis)
          seen    <- server.events.get
          _ = seen shouldBe Nil
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask for a delta against the last result id and apply several edits to it" in {
    val afterFirstDelta  = List(0, 0, 3, 0, 0, 1, 0, 6, 1, 0, 1, 0, 5, 2, 0, 1, 2, 3, 0, 0)
    val afterSecondDelta = List(0, 0, 4, 0, 0, 1, 0, 6, 1, 0, 1, 0, 5, 2, 0)
    runVirtual(
      fakeServer.use { server =>
        for
          _     <- server.announce(fullWithDelta)
          _     <- server.open()
          first <- server.nextRequest
          _ = first.method shouldBe fullMethod
          _ <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _ <- server.awaitEvents(1)

          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ = second.method shouldBe deltaMethod
          _ = second.params.hcursor.downField("previousResultId").as[String].toOption shouldBe Some("1")
          _ <- server.reply(second, deltaResult("2", List((5, 5, List(1, 0, 6, 1, 0)), (15, 0, List(1, 2, 3, 0, 0)))))
          _ <- server.awaitEvents(3)

          _     <- server.edit("object Foo12", 3)
          _     <- server.nextMessage
          third <- server.nextRequest
          _ = third.method shouldBe deltaMethod
          _ = third.params.hcursor.downField("previousResultId").as[String].toOption shouldBe Some("2")
          _    <- server.reply(third, deltaResult("3", List((15, 5, Nil), (2, 1, List(4)))))
          seen <- server.awaitEvents(5)
          _ = seen.collect { case LspEvent.LspSemanticTokensReceived(_, tokens) => tokens } shouldBe List(
            decoded(threeTokens),
            decoded(afterFirstDelta),
            decoded(afterSecondDelta)
          )
          _ <- server.stop
        yield succeed
      }
    )
  }

  it should "accept a full result in answer to a delta request" in
    runVirtual(
      fakeServer.use { server =>
        for
          _      <- server.announce(fullWithDelta)
          _      <- server.open()
          first  <- server.nextRequest
          _      <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _      <- server.awaitEvents(1)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _      <- server.replyWithTokens(second, List(0, 0, 3, 0, 0), resultId = Some("2"))
          seen   <- server.awaitEvents(3)
          _ = seen.lastOption shouldBe Some(received(List(0, 0, 3, 0, 0)))
          _ <- server.stop
        yield succeed
      }
    )

  it should "request the full tokens again when a delta cannot be applied to what it holds" in
    runVirtual(
      fakeServer.use { server =>
        for
          _      <- server.announce(fullWithDelta)
          _      <- server.open()
          first  <- server.nextRequest
          _      <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _      <- server.awaitEvents(1)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ = second.method shouldBe deltaMethod
          _     <- server.reply(second, deltaResult("2", List((100, 0, List(1, 2, 3, 0, 0)))))
          third <- server.nextRequest
          _ = third.method shouldBe fullMethod
          _    <- server.replyWithTokens(third, List(0, 0, 3, 0, 0), resultId = Some("3"))
          seen <- server.awaitEvents(3)
          _ = seen.lastOption shouldBe Some(received(List(0, 0, 3, 0, 0)))
          _ <- server.stop
        yield succeed
      }
    )

  it should "request the full tokens again when the server fails the delta request" in
    runVirtual(
      fakeServer.use { server =>
        for
          _      <- server.announce(fullWithDelta)
          _      <- server.open()
          first  <- server.nextRequest
          _      <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _      <- server.awaitEvents(1)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ <- server.connection.handleIncomingJson(
            io.circe.Json.obj(
              "jsonrpc" -> io.circe.Json.fromString("2.0"),
              "id"      -> io.circe.Json.fromLong(second.id),
              "error" -> io.circe.Json
                .obj("code" -> io.circe.Json.fromInt(-32801), "message" -> io.circe.Json.fromString("content modified"))
            )
          )
          third <- server.nextRequest
          _ = third.method shouldBe fullMethod
          _ <- server.stop
        yield succeed
      }
    )

  it should "fall back to full requests when the server offers no delta" in
    runVirtual(
      fakeServer.use { server =>
        for
          _      <- server.announce(fullOnly)
          _      <- server.open()
          first  <- server.nextRequest
          _      <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _      <- server.awaitEvents(1)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ = second.method shouldBe fullMethod
          _ = second.params.hcursor.downField("previousResultId").focus shouldBe None
          _ <- server.stop
        yield succeed
      }
    )

  it should "fall back to full requests while no result id is held for a delta" in
    runVirtual(
      fakeServer.use { server =>
        for
          _      <- server.announce(fullWithDelta)
          _      <- server.open()
          first  <- server.nextRequest
          _      <- server.replyWithTokens(first, threeTokens, resultId = None)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ = second.method shouldBe fullMethod
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask for the visible lines as well as everything when the server offers range but no delta" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(fullWithRange)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 10, 40))
          _        <- server.open()
          requests <- List.fill(2)(server.nextRequest).sequence
          _     = requests.map(_.method).toSet shouldBe Set(fullMethod, rangeMethod)
          range = requests.find(_.method == rangeMethod)
          _     = range.flatMap(request => positionOf(request.params, "start")) shouldBe Some((10, 0))
          _     = range.flatMap(request => positionOf(request.params, "end")) shouldBe Some((41, 0))
          _ <- server.stop
        yield succeed
      }
    )

  it should "show the visible lines from a range answer that comes before the full one, and the full one over them" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(fullWithRange)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 10, 40))
          _        <- server.open()
          requests <- List.fill(2)(server.nextRequest).sequence
          full  = requests.find(_.method == fullMethod)
          range = requests.find(_.method == rangeMethod)
          _    <- range.traverse_(server.replyWithTokens(_, List(12, 0, 3, 0, 0)))
          _    <- server.awaitEvents(1)
          _    <- full.traverse_(server.replyWithTokens(_, List(0, 0, 3, 0, 0, 12, 0, 3, 0, 0)))
          seen <- server.awaitEvents(2)
          _ = seen shouldBe List(
            LspEvent.LspSemanticTokensRangeReceived(uri, 10, 40, decoded(List(12, 0, 3, 0, 0))),
            received(List(0, 0, 3, 0, 0, 12, 0, 3, 0, 0))
          )
          _ <- server.stop
        yield succeed
      }
    )

  it should "ignore a range answer that comes after the full one for the same version" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(fullWithRange)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 10, 40))
          _        <- server.open()
          requests <- List.fill(2)(server.nextRequest).sequence
          full  = requests.find(_.method == fullMethod)
          range = requests.find(_.method == rangeMethod)
          _    <- full.traverse_(server.replyWithTokens(_, List(0, 0, 3, 0, 0)))
          _    <- server.awaitEvents(1)
          _    <- range.traverse_(server.replyWithTokens(_, List(12, 0, 3, 0, 0)))
          _    <- IO.sleep(20.millis)
          seen <- server.events.get
          _ = seen shouldBe List(received(List(0, 0, 3, 0, 0)))
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask only for the visible lines while the server offers range alone, and again when they scroll" in
    runVirtual(
      fakeServer.use { server =>
        for
          _     <- server.announce(rangeOnly)
          _     <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 5, 20))
          _     <- server.open()
          first <- server.nextRequest
          _ = first.method shouldBe rangeMethod
          _ = positionOf(first.params, "start") shouldBe Some((5, 0))
          _ = positionOf(first.params, "end") shouldBe Some((21, 0))
          _        <- server.replyWithTokens(first, List(6, 0, 3, 0, 0))
          _        <- server.awaitEvents(1)
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 30, 50))
          scrolled <- server.nextRequest
          _ = scrolled.method shouldBe rangeMethod
          _ = positionOf(scrolled.params, "start") shouldBe Some((30, 0))
          _ = positionOf(scrolled.params, "end") shouldBe Some((51, 0))
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask a range-only server for the whole document while the visible lines are unknown" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(rangeOnly)
          _       <- server.open("a\nb\nc")
          request <- server.nextRequest
          _ = request.method shouldBe rangeMethod
          _ = positionOf(request.params, "start") shouldBe Some((0, 0))
          _ = positionOf(request.params, "end") shouldBe Some((3, 0))
          _ <- server.stop
        yield succeed
      }
    )

  it should "not scroll a server that sends the whole document back into a request" in
    runVirtual(
      fakeServer.use { server =>
        for
          _ <- server.announce(fullWithDelta)
          _ <- server.open()
          _ <- server.nextRequest
          _ <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 30, 50))
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "preview the visible lines of a large document while its first full answer is pending" in
    runVirtual(
      fakeServer.use { server =>
        for
          _        <- server.announce(SemanticTokensFeatures(full = true, delta = true, range = true))
          _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _        <- server.open("x\n" * 6000)
          requests <- List.fill(2)(server.nextRequest).sequence
          _ = requests.map(_.method).toSet shouldBe Set(fullMethod, rangeMethod)
          _ <- server.stop
        yield succeed
      }
    )

  it should "ask a small document only for a delta once it holds a result id, with no preview" in
    runVirtual(
      fakeServer.use { server =>
        for
          _     <- server.announce(SemanticTokensFeatures(full = true, delta = true, range = true))
          _     <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _     <- server.open()
          first <- server.nextRequest
          _ = first.method shouldBe fullMethod
          _      <- server.replyWithTokens(first, threeTokens, resultId = Some("1"))
          _      <- server.awaitEvents(1)
          _      <- server.edit("object Foo1", 2)
          _      <- server.nextMessage
          second <- server.nextRequest
          _ = second.method shouldBe deltaMethod
          _ <- server.assertQuietFor(2.seconds)
          _ <- server.stop
        yield succeed
      }
    )

  it should "tell the state how the text changed while tokens are shown, so they can move with it" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(fullOnly)
          _       <- server.open()
          request <- server.nextRequest
          _       <- server.replyWithTokens(request, List(0, 0, 3, 0, 0))
          _       <- server.awaitEvents(1)
          _       <- server.edit("\nobject Foo", 2)
          seen    <- server.awaitEvents(2)
          _ = seen.lastOption shouldBe Some(
            LspEvent.LspSemanticTokensEdited(
              uri,
              TextChangeDiff.Change(LspRange(LspPosition(0, 0), LspPosition(0, 0)), 0, "\n")
            )
          )
          _ <- server.stop
        yield succeed
      }
    )

  it should "not report edits while no tokens are shown" in
    runVirtual(
      fakeServer.use { server =>
        for
          _    <- server.announce(fullOnly)
          _    <- server.open()
          _    <- server.nextRequest
          _    <- server.edit("\nobject Foo", 2)
          _    <- IO.sleep(20.millis)
          seen <- server.events.get
          _ = seen shouldBe Nil
          _ <- server.stop
        yield succeed
      }
    )
