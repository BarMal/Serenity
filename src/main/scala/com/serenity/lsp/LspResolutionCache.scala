package com.serenity.lsp

import cats.effect.{IO, Ref}
import com.serenity.lsp.client.{DocumentUri, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerConfig}

/** Memoizes the (server config, workspace root) resolution per (language, document), so the filesystem/PATH work in
  * WorkspaceRootDetector and LspServerRegistry is paid once per open document instead of on every request.
  */
final private[lsp] class LspResolutionCache private (
    ref: Ref[IO, Map[LspResolutionCache.Key, Option[(LspServerConfig, WorkspaceRootUri)]]]
):
  import LspResolutionCache.Key

  def resolve(languageId: LanguageId, fileUri: DocumentUri)(
    compute: IO[Option[(LspServerConfig, WorkspaceRootUri)]]
  ): IO[Option[(LspServerConfig, WorkspaceRootUri)]] =
    val key = Key(languageId, fileUri)
    ref
      .modify(map => (map, map.get(key)))
      .flatMap {
        case Some(cached) => IO.pure(cached)
        case None         => compute.flatTap(result => ref.update(_.updated(key, result)))
      }

  /** Drop the cached resolution for a (languageId, fileUri), so the next `resolve` for it recomputes rather than
    * reusing a resolution left over from before the document closed.
    */
  def evict(languageId: LanguageId, fileUri: DocumentUri): IO[Unit] =
    ref.update(_ - Key(languageId, fileUri))

private[lsp] object LspResolutionCache:
  final private case class Key(languageId: LanguageId, fileUri: DocumentUri)

  def empty: IO[LspResolutionCache] =
    Ref.of[IO, Map[Key, Option[(LspServerConfig, WorkspaceRootUri)]]](Map.empty).map(new LspResolutionCache(_))
