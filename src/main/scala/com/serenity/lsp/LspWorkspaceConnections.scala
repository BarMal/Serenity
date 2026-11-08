package com.serenity.lsp

import java.nio.file.{Path, Paths}

import cats.effect.IO
import com.serenity.lsp.LspManager.{ConnectionIdentity, ResolvedConnection, TrustChange}
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerRegistry, LspUserConfig, WorkspaceRootDetector}
import com.serenity.trust.CommandSource
import org.typelevel.log4cats.Logger

/** The real [[LspManager.ConnectionProvider]]: a server from the registry, for the workspace root the file lies in,
  * started only when that root is trusted ([[LspTrustGate]]).
  */
private[lsp] object LspWorkspaceConnections:

  def provider(
    userConfig: LspUserConfig,
    logger: Logger[IO],
    resolutionCache: LspResolutionCache,
    gate: LspTrustGate
  ): LspManager.ConnectionProvider =
    new LspManager.ConnectionProvider:
      def resolve(
        languageId: LanguageId,
        fileUri: DocumentUri,
        onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
      ): IO[Option[ResolvedConnection]] =
        resolutionCache
          .resolve(languageId, fileUri) {
            LspServerRegistry.resolve(languageId, userConfig).flatMap {
              case None =>
                logger.info(s"[LSP] No server available for ${languageId.id}").as(None)
              case Some(config) =>
                workspaceRoot(languageId, fileUri).flatMap { (root, rootUri) =>
                  val source =
                    if userConfig.servers.exists(_.contains(languageId.id)) then CommandSource.UserLayer
                    else CommandSource.BuiltIn
                  gate.admit(root, config, source).flatMap { admitted =>
                    if admitted then IO.pure(Some(config -> rootUri))
                    else logger.info(s"[LSP] ${config.command} not started: $root is not trusted").as(None)
                  }
                }
            }
          }
          .map {
            case None => None
            case Some((config, rootUri)) =>
              Some(ResolvedConnection(ConnectionIdentity(rootUri, config), LspConnection(config, rootUri, logger)))
          }

      override def evictResolution(languageId: LanguageId, fileUri: DocumentUri): IO[Unit] =
        resolutionCache.evict(languageId, fileUri)

      override def evictAllResolutions: IO[Unit] = resolutionCache.evictAll

      override def trustFolderOf(languageId: LanguageId, fileUri: DocumentUri): IO[TrustChange] =
        workspaceRoot(languageId, fileUri).flatMap((root, _) => gate.trustFolder(root)).map { key =>
          TrustChange(s"Trusted ${key.path}: its language servers can start.", revoked = Nil)
        }

      override def revokeTrustOf(languageId: LanguageId, fileUri: DocumentUri): IO[TrustChange] =
        workspaceRoot(languageId, fileUri).flatMap { (root, _) =>
          gate.revokeCovering(root).map {
            case Nil => TrustChange(s"$root is not in a trusted folder.", revoked = Nil)
            case revoked =>
              TrustChange(
                s"Revoked trust for ${revoked.map(_.path).mkString(", ")}: its language servers have stopped.",
                revoked = revoked.map(_.path)
              )
          }
        }

  /** The root a server for `fileUri` would serve: its project root, or the file's own folder when there is none. */
  private def workspaceRoot(languageId: LanguageId, fileUri: DocumentUri): IO[(Path, WorkspaceRootUri)] =
    val filePath = uriToPath(fileUri)
    WorkspaceRootDetector.detect(filePath, languageId).map { rootOpt =>
      val rootUri = rootOpt.map(r => WorkspaceRootUri(r.toUri.toString)).getOrElse(parentUri(fileUri))
      val root    = rootOpt.orElse(Option(Paths.get(filePath).getParent)).getOrElse(Paths.get(filePath))
      (root, rootUri)
    }

  private def uriToPath(uri: DocumentUri): String =
    val s = uri.value
    if s.startsWith("file://") then java.net.URI.create(s).getPath
    else s

  private def parentUri(uri: DocumentUri): WorkspaceRootUri =
    val s         = uri.value
    val lastSlash = s.lastIndexOf('/')
    WorkspaceRootUri(if lastSlash > 0 then s.substring(0, lastSlash) else s)
