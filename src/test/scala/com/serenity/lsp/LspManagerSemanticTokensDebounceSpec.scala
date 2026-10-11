package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Supervisor
import cats.effect.{Fiber, IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.lsp.model.SemanticTokensFeatures
import com.serenity.state.effects.EffectLanes
import com.serenity.testkit.VirtualTime.runVirtual
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec

/** What the semantic tokens debounce costs while the text keeps changing: no fiber per call, and no unbounded wait for
  * the first request of a burst that never goes quiet.
  */
class LspManagerSemanticTokensDebounceSpec extends AnyFlatSpec with SemanticTokensFakeServer:

  private val fullMethod  = "textDocument/semanticTokens/full"
  private val rangeMethod = "textDocument/semanticTokens/range"

  private def countingSupervisor(started: Ref[IO, Int])(underlying: Supervisor[IO]): Supervisor[IO] =
    new Supervisor[IO]:
      def supervise[A](fa: IO[A]): IO[Fiber[IO, Throwable, A]] = started.update(_ + 1) >> underlying.supervise(fa)

  private def fibersStartedBy(calls: Int, gap: FiniteDuration, served: Option[LspConnection] = None)(
    call: LspManagerSemanticTokens => IO[Unit]
  ): Int =
    runVirtual(
      (Supervisor[IO](await = false), EffectLanes.resource((_, _) => IO.unit)).tupled.use { (real, lanes) =>
        for
          started          <- Ref.of[IO, Int](0)
          documentVersions <- Ref.of[IO, Map[DocumentUri, Int]](Map.empty)
          openDocuments    <- Ref.of[IO, OpenDocument.Registry](Map.empty)
          requestContexts  <- Ref.of[IO, Map[LspManager.RequestKey, LspManager.RequestContext]](Map.empty)
          requestFibers    <- Ref.of[IO, Map[LspManager.RequestKey, Fiber[IO, Throwable, Unit]]](Map.empty)
          supervisor = countingSupervisor(started)(real)
          serverNotices <- LspServerNotices.create(LspNotices.ignoring)
          provider = new LspManager.ConnectionProvider:
            def resolve(
              languageId: LanguageId,
              fileUri: DocumentUri,
              onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
            ): IO[Option[LspManager.ResolvedConnection]] =
              IO.pure(served.map { connection =>
                LspManager.ResolvedConnection(
                  LspManager.ConnectionIdentity(
                    WorkspaceRootUri("file:///workspace"),
                    LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)
                  ),
                  Resource.eval(IO.pure(connection))
                )
              })
          pool <- LspConnectionPool.create(
            documentVersions,
            openDocuments,
            supervisor,
            provider,
            LspSupervisionPolicy.Default,
            serverNotices,
            _ => IO.unit,
            logger
          )
          session <- SemanticTokensSession.create
          tokens = LspManagerSemanticTokens(
            pool,
            documentVersions,
            openDocuments,
            requestContexts,
            requestFibers,
            supervisor,
            _ => IO.unit,
            logger,
            session,
            lanes,
            LspManagerSemanticTokens.DefaultDebounce
          )
          _ <- documentVersions.update(_.updated(DocumentUri(uri), 1))
          _ <- served.traverse_(_ =>
            pool.attach(LanguageId.Scala, DocumentUri(uri)).flatMap {
              case LspAttachment.Live(identity, _) => pool.associate(DocumentUri(uri), identity)
              case _                               => IO.unit
            }
          )
          before <- started.get
          _      <- (call(tokens) >> IO.sleep(gap)).replicateA_(calls)
          _      <- IO.sleep(5.seconds)
          count  <- started.get
        yield count - before
      }
    )

  private def drainOutgoing(server: FakeServer): IO[List[Json]] =
    server.connection.tryTakeOutgoing.flatMap {
      case Some(Some(message)) => drainOutgoing(server).map(message :: _)
      case Some(None) | None   => IO.pure(Nil)
    }

  "The semantic tokens debounce" should "start no fiber per call while edits keep arriving" in {
    val started = fibersStartedBy(200, 10.millis)(_.refreshAfterQuiet(uri, LanguageId.Scala))
    started should be <= 2
  }

  it should "start no fiber per call while the viewport keeps scrolling" in {
    val connection = runVirtual(
      LspConnection
        .create(LanguageId.Scala, logger)
        .flatTap(_.recordSemanticTokensLegend(Some(legend)))
        .flatTap(
          _.recordSemanticTokensFeatures(fullWithRange)
        )
    )
    val started =
      fibersStartedBy(200, 10.millis, Some(connection))(_.visibleRangeChanged(uri, LanguageId.Scala, 0, 40))
    started should be <= 4
  }

  it should "request tokens within a bounded time of the first edit while edits never pause for the debounce delay" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(SemanticTokensFeatures.FullOnly)
          _       <- server.open()
          opening <- server.nextRequest
          _       <- server.replyWithTokens(opening, List(0, 0, 3, 0, 0))
          _       <- server.awaitEvents(1)
          firstAt <- IO.monotonic
          requestTimes <- (1 to 30).toList.flatTraverse { n =>
            for
              _        <- server.edit(s"object Foo$n", n + 1)
              _        <- IO.sleep(100.millis)
              now      <- IO.monotonic
              messages <- drainOutgoing(server)
            yield messages.filter(server.methodOf(_) == fullMethod).as(now - firstAt)
          }
          _ = requestTimes should not be empty
          _ = requestTimes.headOption.exists(_ <= 1500.millis) shouldBe true
          _ = requestTimes.size should be >= 2
          _ <- server.stop
        yield succeed
      }
    )

  it should "request the visible lines within a bounded time while the viewport never rests for the debounce delay" in
    runVirtual(
      fakeServer.use { server =>
        for
          _       <- server.announce(rangeOnly)
          _       <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 30))
          _       <- server.open()
          opening <- server.nextRequest
          _       <- server.replyWithTokens(opening, List(0, 0, 3, 0, 0))
          _       <- server.awaitEvents(1)
          firstAt <- IO.monotonic
          requestTimes <- (1 to 30).toList.flatTraverse { n =>
            for
              _        <- server.send(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, n * 10, n * 10 + 30))
              _        <- IO.sleep(100.millis)
              now      <- IO.monotonic
              messages <- drainOutgoing(server)
            yield messages.filter(server.methodOf(_) == rangeMethod).as(now - firstAt)
          }
          _ = requestTimes.headOption.exists(_ <= 1500.millis) shouldBe true
          _ = requestTimes.size should be >= 2
          _ <- server.stop
        yield succeed
      }
    )
