package com.serenity.lsp

import scala.concurrent.duration.FiniteDuration

import cats.effect.std.{Mutex, Supervisor}
import cats.effect.{IO, Ref, Unique}
import cats.syntax.all.*
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.LspManager.{ConnectionIdentity, ConnectionProvider, ManagedConnection, ResolvedConnection}
import com.serenity.lsp.OpenDocument.textOf
import com.serenity.lsp.client.{
  DocumentUri,
  LspApplyEditRequest,
  LspApplyEditResult,
  LspClientHooks,
  LspConnection,
  LspMessageLevel,
  LspMethod,
  LspProtocol,
  LspServerMessage
}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import io.circe.Json
import org.typelevel.log4cats.Logger

private[lsp] enum LspAttachment:
  case Live(identity: ConnectionIdentity, connection: LspConnection)
  case Suspended(identity: ConnectionIdentity)
  case NoServer

/** The language-server connections `LspManager` holds, which open document each one serves, and their lifecycle under
  * an [[LspSupervisionPolicy]]: a server that dies is released and restarted with its documents reopened, and a server
  * left serving nothing is shut down after a grace period.
  *
  * Crash handling and idle shutdown happen on their own fibers, so every change to this state runs under
  * [[serialized]], the same lock `LspManager` holds while it handles an effect.
  */
final private[lsp] class LspConnectionPool private (
    connections: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    documents: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    crashes: Ref[IO, Map[ConnectionIdentity, List[FiniteDuration]]],
    idleSince: Ref[IO, Map[ConnectionIdentity, Unique.Token]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    openDocuments: Ref[IO, OpenDocument.Registry],
    lock: Mutex[IO],
    supervisor: Supervisor[IO],
    provider: ConnectionProvider,
    policy: LspSupervisionPolicy,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
):

  def serialized[A](work: IO[A]): IO[A] = lock.lock.surround(work)

  def connectionFor(uri: DocumentUri): IO[Option[LspConnection]] =
    (documents.get, connections.get).mapN((docs, live) => docs.get(uri).flatMap(live.get).map(_.connection))

  def evictResolution(languageId: LanguageId, uri: DocumentUri): IO[Unit] =
    provider.evictResolution(languageId, uri)

  /** The connection serving `uri`, starting one if its server is not running -- unless that server is down and still
    * inside the backoff or give-up its crashes earned, in which case the document waits for the restart.
    */
  def attach(languageId: LanguageId, uri: DocumentUri): IO[LspAttachment] =
    provider.resolve(languageId, uri, onDiagnostics).flatMap {
      case None => IO.pure(LspAttachment.NoServer)
      case Some(resolved) =>
        connections.get.map(_.get(resolved.identity)).flatMap {
          case Some(managed) => IO.pure(LspAttachment.Live(resolved.identity, managed.connection))
          case None =>
            mayConnect(resolved.identity).ifM(spawn(resolved), IO.pure(LspAttachment.Suspended(resolved.identity)))
        }
    }

  /** Whether a server, running or down, is expected to serve `uri`: unlike having a connection, this holds while the
    * server waits out a restart backoff.
    */
  def hasServer(uri: DocumentUri): IO[Boolean] =
    documents.get.map(_.contains(uri))

  def associate(uri: DocumentUri, identity: ConnectionIdentity): IO[Unit] =
    idleSince.update(_ - identity) >>
      documents
        .modify(docs => (docs.updated(uri, identity), docs.get(uri).filter(_ != identity)))
        .flatMap(_.traverse_(shutDownWhenIdle))

  def release(uri: DocumentUri): IO[Unit] =
    documents.modify(docs => (docs - uri, docs.get(uri))).flatMap(_.traverse_(shutDownWhenIdle))

  def releaseAll: IO[Unit] =
    connections.modify(live => (Map.empty, live.values.toList)).flatMap(_.traverse_(releaseQuietly))

  private val onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit] =
    (uri, diagnostics) => applyEvent(LspEvent.LspDiagnosticsReceived(uri.value, diagnostics))

  private def hooksFor(identity: ConnectionIdentity): LspClientHooks =
    val languageId = identity.serverConfig.languageId
    LspClientHooks(
      onMessage = logServerMessage(identity, _),
      onProgress = update => applyEvent(LspEvent.LspProgressReceived(languageId, update.token, update.progress)),
      onApplyEdit = applyServerEdit
    )

  private def logServerMessage(identity: ConnectionIdentity, message: LspServerMessage): IO[Unit] =
    val line = s"[LSP] ${serverName(identity)}: ${message.text}"
    message.level match
      case LspMessageLevel.Error   => logger.error(line)
      case LspMessageLevel.Warning => logger.warn(line)
      case LspMessageLevel.Info    => logger.info(line)
      case LspMessageLevel.Log     => logger.debug(line)

  /** Only documents the server was told about can be edited: it has no business changing a file this editor never
    * opened, and the edit is all-or-nothing, as `workspace/applyEdit` defaults to.
    */
  private def applyServerEdit(request: LspApplyEditRequest): IO[LspApplyEditResult] =
    openDocuments.get.flatMap { open =>
      request.edits.keys.filterNot(open.contains).toList match
        case Nil =>
          applyEvent(LspEvent.LspWorkspaceEditRequested(request.edits.map((uri, edits) => uri.value -> edits)))
            .as(LspApplyEditResult(applied = true, None))
        case unopened =>
          IO.pure(
            LspApplyEditResult(
              applied = false,
              Some(s"Not open in the editor: ${unopened.map(_.value).mkString(", ")}")
            )
          )
    }

  private def spawn(resolved: ResolvedConnection): IO[LspAttachment] =
    resolved.resource.allocated.attempt.flatMap {
      case Left(error) =>
        logger.error(error)(s"[LSP] Failed to connect for ${serverName(resolved.identity)}") >>
          recordCrash(resolved).as(LspAttachment.Suspended(resolved.identity))
      case Right((connection, release)) =>
        val hooks = hooksFor(resolved.identity)
        (connection.useHooks(hooks) >> connection.processIncoming(onDiagnostics, hooks).start).flatMap {
          diagnosticsFiber =>
            val managed = ManagedConnection(connection, release >> diagnosticsFiber.cancel)
            connections.update(_.updated(resolved.identity, managed)) >>
              supervisor.supervise(connection.terminated >> serialized(onTerminated(resolved, connection))) >>
              reopenDocuments(resolved.identity, connection).as(LspAttachment.Live(resolved.identity, connection))
        }
    }

  /** A connection released on purpose was removed from `connections` first, so only a server that died while still in
    * use is found here.
    */
  private def onTerminated(resolved: ResolvedConnection, connection: LspConnection): IO[Unit] =
    connections
      .modify { live =>
        live.get(resolved.identity).filter(_.connection eq connection) match
          case Some(managed) => (live - resolved.identity, Some(managed))
          case None          => (live, None)
      }
      .flatMap(_.traverse_ { managed =>
        logger.warn(s"[LSP] ${serverName(resolved.identity)} server stopped unexpectedly") >>
          clearProgress(resolved.identity) >>
          inBackground(releaseQuietly(managed)) >>
          recordCrash(resolved)
      })

  private def recordCrash(resolved: ResolvedConnection): IO[Unit] =
    IO.monotonic.flatMap { now =>
      crashes
        .modify { all =>
          val recent = now :: policy.recentCrashes(all.getOrElse(resolved.identity, Nil), now)
          (all.updated(resolved.identity, recent), recent)
        }
        .flatMap { recent =>
          policy.afterCrash(recent, now) match
            case LspRestartDecision.RestartAfter(delay) =>
              supervisor.supervise(IO.sleep(delay) >> serialized(restart(resolved))).void
            case LspRestartDecision.GiveUp(crashCount) =>
              warnDocuments(resolved.identity, crashCount)
        }
    }

  /** Nothing to do when a demand already restarted the server during the backoff, or its documents all closed. */
  private def restart(resolved: ResolvedConnection): IO[Unit] =
    (connections.get.map(_.contains(resolved.identity)), isReferenced(resolved.identity)).flatMapN {
      case (false, true) => spawn(resolved).void
      case _             => IO.unit
    }

  private def mayConnect(identity: ConnectionIdentity): IO[Boolean] =
    (crashes.get, IO.monotonic).mapN((all, now) => policy.mayConnect(all.getOrElse(identity, Nil), now))

  private def reopenDocuments(identity: ConnectionIdentity, connection: LspConnection): IO[Unit] =
    (documentsOf(identity), documentVersions.get, openDocuments.get).flatMapN { (uris, versions, open) =>
      uris.traverse_ { uri =>
        open.textOf(uri).traverse_ { text =>
          connection
            .sendNotification(
              LspMethod("textDocument/didOpen"),
              LspProtocol
                .didOpenParams(uri, identity.serverConfig.languageId.id, versions.getOrElse(uri, 1), text.collect())
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] reopening ${uri.value} failed"))
        }
      }
    }

  /** A warning diagnostic rather than a dialog: it sits with the affected files, interrupts nothing, and the next
    * diagnostics a restarted server publishes replace it.
    */
  private def warnDocuments(identity: ConnectionIdentity, crashCount: Int): IO[Unit] =
    val message =
      s"The ${serverName(identity)} language server stopped after crashing $crashCount times in " +
        s"${policy.restartWindow.toCoarsest}. Language features for this file are paused until it can be restarted."
    val warning = Diagnostic(
      range = LspRange(LspPosition(0, 0), LspPosition(0, 0)),
      severity = Some(DiagnosticSeverity.Warning),
      message = message,
      source = Some("Serenity")
    )
    logger.warn(s"[LSP] $message") >>
      documentsOf(identity).flatMap(
        _.traverse_(uri => applyEvent(LspEvent.LspDiagnosticsReceived(uri.value, List(warning))))
      )

  private def shutDownWhenIdle(identity: ConnectionIdentity): IO[Unit] =
    isReferenced(identity).ifM(
      IO.unit,
      IO.unique.flatMap { token =>
        idleSince.update(_.updated(identity, token)) >>
          supervisor
            .supervise(IO.sleep(policy.idleShutdownGrace) >> serialized(shutDownIfStillIdle(identity, token)))
            .void
      }
    )

  private def shutDownIfStillIdle(identity: ConnectionIdentity, token: Unique.Token): IO[Unit] =
    idleSince
      .modify(idle => if idle.get(identity).contains(token) then (idle - identity, true) else (idle, false))
      .ifM(
        connections
          .modify(live => (live - identity, live.get(identity)))
          .flatMap(_.traverse_(managed => inBackground(shutDown(identity, managed)))),
        IO.unit
      )

  /** `shutdown` then `exit`, as the protocol asks, before the process is released; a server that does not answer within
    * `shutdownTimeout` is released regardless.
    */
  private def shutDown(identity: ConnectionIdentity, managed: ManagedConnection): IO[Unit] =
    logger.info(s"[LSP] Shutting down idle ${serverName(identity)} server") >>
      clearProgress(identity) >>
      managed.connection.sendRequest(LspMethod("shutdown"), Json.Null, policy.shutdownTimeout).attempt >>
      managed.connection.sendNotification(LspMethod("exit"), Json.Null).attempt >>
      releaseQuietly(managed)

  /** Uncancelable so stopping the manager waits for a release in flight instead of abandoning the process. */
  private def inBackground(work: IO[Unit]): IO[Unit] =
    supervisor.supervise(work.uncancelable).void

  private def releaseQuietly(managed: ManagedConnection): IO[Unit] =
    managed.release.handleErrorWith(ex => logger.error(ex)("[LSP] release failed"))

  private def clearProgress(identity: ConnectionIdentity): IO[Unit] =
    applyEvent(LspEvent.LspServerStopped(identity.serverConfig.languageId))

  private def isReferenced(identity: ConnectionIdentity): IO[Boolean] =
    documents.get.map(_.values.exists(_ == identity))

  private def documentsOf(identity: ConnectionIdentity): IO[List[DocumentUri]] =
    documents.get.map(_.collect { case (uri, owner) if owner == identity => uri }.toList)

  private def serverName(identity: ConnectionIdentity): String =
    identity.serverConfig.languageId.displayName

private[lsp] object LspConnectionPool:

  def create(
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    openDocuments: Ref[IO, OpenDocument.Registry],
    supervisor: Supervisor[IO],
    provider: ConnectionProvider,
    policy: LspSupervisionPolicy,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
  ): IO[LspConnectionPool] =
    for
      connections <- Ref.of[IO, Map[ConnectionIdentity, ManagedConnection]](Map.empty)
      documents   <- Ref.of[IO, Map[DocumentUri, ConnectionIdentity]](Map.empty)
      crashes     <- Ref.of[IO, Map[ConnectionIdentity, List[FiniteDuration]]](Map.empty)
      idleSince   <- Ref.of[IO, Map[ConnectionIdentity, Unique.Token]](Map.empty)
      lock        <- Mutex[IO]
    yield new LspConnectionPool(
      connections,
      documents,
      crashes,
      idleSince,
      documentVersions,
      openDocuments,
      lock,
      supervisor,
      provider,
      policy,
      applyEvent,
      logger
    )
