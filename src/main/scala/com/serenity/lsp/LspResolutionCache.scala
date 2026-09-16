package com.serenity.lsp

import cats.effect.{IO, Ref}
import com.serenity.lsp.client.{DocumentUri, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerConfig}

/** Memoizes the (server config, workspace root) resolution per (language, document), so the filesystem/PATH work in
  * WorkspaceRootDetector and LspServerRegistry is paid once per open document instead of on every request.
  */
final private[lsp] class LspResolutionCache private (
    ref: Ref[IO, Map[LspResolutionCache.Key, IO[Option[(LspServerConfig, WorkspaceRootUri)]]]]
):
  import LspResolutionCache.Key

  /** Reads, decides whether to compute, and records the (memoized) computation as a single atomic `ref.modify` step,
    * so two `resolve` calls racing for the same key can never both observe "not yet cached" and both run `compute` --
    * a non-atomic read-then-write (a `ref.modify(get)` followed by a separate `ref.update`) leaves exactly that window
    * open (#1451). `compute.memoize` (not `compute` itself) is what gets stored: it shares one in-flight computation
    * (and its eventual result) across every caller that flattens the same stored `IO`, whether they raced in before it
    * completed or arrived after.
    */
  def resolve(languageId: LanguageId, fileUri: DocumentUri)(
    compute: IO[Option[(LspServerConfig, WorkspaceRootUri)]]
  ): IO[Option[(LspServerConfig, WorkspaceRootUri)]] =
    val key = Key(languageId, fileUri)
    compute.memoize.flatMap { memoized =>
      ref.modify(map => map.get(key).fold((map.updated(key, memoized), memoized))(existing => (map, existing)))
    }.flatten

  /** Drop the cached resolution for a (languageId, fileUri), so the next `resolve` for it recomputes rather than
    * reusing a resolution left over from before the document closed.
    */
  def evict(languageId: LanguageId, fileUri: DocumentUri): IO[Unit] =
    ref.update(_ - Key(languageId, fileUri))

private[lsp] object LspResolutionCache:
  final private case class Key(languageId: LanguageId, fileUri: DocumentUri)

  def empty: IO[LspResolutionCache] =
    Ref.of[IO, Map[Key, IO[Option[(LspServerConfig, WorkspaceRootUri)]]]](Map.empty).map(new LspResolutionCache(_))
