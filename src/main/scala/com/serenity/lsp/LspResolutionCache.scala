package com.serenity.lsp

import cats.effect.{IO, Ref}
import com.serenity.lsp.config.{LanguageId, LspServerConfig}

/** Memoizes the (server config, workspace root) resolution per (language, document), so the filesystem/PATH work in
  * WorkspaceRootDetector and LspServerRegistry is paid once per open document instead of on every request.
  */
final private[lsp] class LspResolutionCache private (
    ref: Ref[IO, Map[LspResolutionCache.Key, Option[(LspServerConfig, String)]]]
):
  import LspResolutionCache.Key

  def resolve(languageId: LanguageId, fileUri: String)(
    compute: IO[Option[(LspServerConfig, String)]]
  ): IO[Option[(LspServerConfig, String)]] =
    val key = Key(languageId, fileUri)
    ref.get.map(_.get(key)).flatMap {
      case Some(cached) => IO.pure(cached)
      case None         => compute.flatTap(result => ref.update(_.updated(key, result)))
    }

private[lsp] object LspResolutionCache:
  private case class Key(languageId: LanguageId, fileUri: String)

  def empty: IO[LspResolutionCache] =
    Ref.of[IO, Map[Key, Option[(LspServerConfig, String)]]](Map.empty).map(new LspResolutionCache(_))
