package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.{Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.Event
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.lsp.model.{SemanticTokensFeatures, SemanticTokensLegend}
import com.serenity.testkit.RopeText
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** A language server for specs of the semantic tokens requests: an in-memory connection the test drives from the server
  * side, announcing the requests it supports, receiving what the manager sends and replying to it, under whatever clock
  * the spec runs on.
  */
private[lsp] trait SemanticTokensFakeServer extends Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  protected val logger: Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("SemanticTokensFakeServer"))
  protected val uri: String        = "file:///workspace/Foo.scala"

  protected val legend: SemanticTokensLegend = SemanticTokensLegend(List("keyword", "string", "comment"), Nil)

  protected val fullWithDelta: SemanticTokensFeatures = SemanticTokensFeatures(full = true, delta = true, range = false)
  protected val fullWithRange: SemanticTokensFeatures = SemanticTokensFeatures(full = true, delta = false, range = true)
  protected val rangeOnly: SemanticTokensFeatures = SemanticTokensFeatures(full = false, delta = false, range = true)

  /** A request the manager sent: its id, method and params. */
  final protected case class Request(id: Long, method: String, params: Json)

  final protected case class FakeServer(
      effects: Queue[IO, Option[LspEffect]],
      events: Ref[IO, List[Event]],
      connection: LspConnection,
      managerFiber: Fiber[IO, Throwable, Unit]
  ):

    def stop: IO[Unit] = effects.offer(None) >> managerFiber.joinWithNever

    def announce(features: SemanticTokensFeatures): IO[Unit] =
      connection.recordSemanticTokensLegend(Some(legend)) >> connection.recordSemanticTokensFeatures(features)

    def send(effect: LspEffect): IO[Unit] = effects.offer(Some(effect))

    def open(text: String = "object Foo"): IO[Unit] =
      send(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText(text))) >> nextMessage.void

    def edit(text: String, version: Int): IO[Unit] =
      send(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText(text), version))

    def nextMessage: IO[Json] =
      connection.takeOutgoing.flatMap(IO.fromOption(_)(new RuntimeException("The connection closed")))

    def methodOf(message: Json): String = message.hcursor.downField("method").as[String].getOrElse("")

    /** The next request, whatever notifications precede it. */
    def nextRequest: IO[Request] =
      nextMessage.flatMap { message =>
        message.hcursor.downField("id").as[Long].toOption match
          case Some(id) =>
            IO.pure(Request(id, methodOf(message), message.hcursor.downField("params").focus.getOrElse(Json.Null)))
          case None => nextRequest
      }

    def reply(request: Request, result: Json): IO[Unit] =
      connection.handleIncomingJson(
        Json.obj("jsonrpc" -> "2.0".asJson, "id" -> request.id.asJson, "result" -> result)
      )

    def replyWithTokens(request: Request, data: List[Int], resultId: Option[String] = None): IO[Unit] =
      reply(request, tokensResult(data, resultId))

    def assertQuietFor(duration: FiniteDuration): IO[Unit] =
      IO.sleep(duration) >> connection.tryTakeOutgoing.map(_ shouldBe None)

    def awaitEvents(count: Int): IO[List[Event]] =
      def poll: IO[List[Event]] =
        events.get.flatMap(seen => if seen.size >= count then IO.pure(seen) else IO.sleep(10.millis) >> poll)
      poll.timeout(30.seconds)

  protected def tokensResult(data: List[Int], resultId: Option[String]): Json =
    Json.obj("data" -> data.asJson).deepMerge(resultId.fold(Json.obj())(id => Json.obj("resultId" -> id.asJson)))

  protected def deltaResult(resultId: String, edits: List[(Int, Int, List[Int])]): Json =
    Json.obj(
      "resultId" -> resultId.asJson,
      "edits" -> edits.map { (start, deleteCount, data) =>
        Json.obj("start" -> start.asJson, "deleteCount" -> deleteCount.asJson, "data" -> data.asJson)
      }.asJson
    )

  protected def fakeServer: Resource[IO, FakeServer] =
    for
      effects    <- Resource.eval(Queue.unbounded[IO, Option[LspEffect]])
      events     <- Resource.eval(Ref.of[IO, List[Event]](Nil))
      connection <- Resource.eval(LspConnection.create(LanguageId.Scala, logger))
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(
            Some(
              LspManager.ResolvedConnection(
                LspManager.ConnectionIdentity(
                  WorkspaceRootUri("file:///workspace"),
                  LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)
                ),
                Resource.eval(IO.pure(connection))
              )
            )
          )
      managerFiber <- Resource.make(
        LspManager
          .runWithProvider(
            Stream.fromQueueNoneTerminated(effects),
            event => events.update(_ :+ event),
            logger,
            provider
          )
          .start
      )(_.cancel)
    yield FakeServer(effects, events, connection, managerFiber)
