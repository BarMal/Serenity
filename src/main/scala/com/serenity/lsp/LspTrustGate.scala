package com.serenity.lsp

import java.nio.file.Path

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.lsp.client.{LspMessageAction, LspMessageLevel, LspMessageRequest}
import com.serenity.lsp.config.LspServerConfig
import com.serenity.trust.{CommandSource, ProjectKey, ProjectTrustAccess}
import io.circe.Json

/** Whether a language server may start for a workspace root (#2526, owner decision (a)). Starting even a stock server
  * imports the project's build and so runs its code, so an untrusted root keeps its servers off. The first refusal for
  * a root asks, once per session, whether to trust the folder; accepting calls `onTrusted` so its documents can be
  * announced again.
  */
final private[lsp] class LspTrustGate private (
    trust: ProjectTrustAccess,
    ask: (String, LspMessageRequest) => IO[Option[Int]],
    supervisor: Supervisor[IO],
    asked: Ref[IO, Set[ProjectKey]],
    onTrusted: ProjectKey => IO[Unit]
):

  def admit(root: Path, config: LspServerConfig, source: CommandSource): IO[Boolean] =
    (ProjectKey.resolve(root), trust.current).tupled.flatMap { (key, current) =>
      current.grant(key, source, config.command :: config.defaultArgs) match
        case Right(_) => IO.pure(true)
        case Left(_) =>
          asked
            .modify(roots => (roots + key, !roots.contains(key)))
            .flatMap(first => supervisor.supervise(offerTrust(key, config)).void.whenA(first))
            .as(false)
    }

  /** What accepting the notice does, asked for from the palette instead. */
  def trustFolder(root: Path): IO[ProjectKey] =
    ProjectKey.resolve(root).flatTap(key => trust.trustFolder(key) >> onTrusted(key))

  def revokeCovering(root: Path): IO[List[ProjectKey]] =
    ProjectKey.resolve(root).flatMap(trust.revokeCovering)

  private def offerTrust(key: ProjectKey, config: LspServerConfig): IO[Unit] =
    ask(config.command, LspTrustGate.question(key)).flatMap {
      case Some(0) => trust.trustFolder(key) >> onTrusted(key)
      case _       => IO.unit
    }

private[lsp] object LspTrustGate:

  val TrustAction: String = "Trust this folder"

  def create(
    trust: ProjectTrustAccess,
    ask: (String, LspMessageRequest) => IO[Option[Int]],
    supervisor: Supervisor[IO],
    onTrusted: ProjectKey => IO[Unit]
  ): IO[LspTrustGate] =
    Ref.of[IO, Set[ProjectKey]](Set.empty).map(new LspTrustGate(trust, ask, supervisor, _, onTrusted))

  private def question(key: ProjectKey): LspMessageRequest =
    LspMessageRequest(
      LspMessageLevel.Warning,
      s"off, because ${key.path} is not a trusted folder. Trusting it lets Serenity run language servers and builds " +
        "there, which run the project's own code.",
      List(LspMessageAction(TrustAction, Json.Null))
    )
