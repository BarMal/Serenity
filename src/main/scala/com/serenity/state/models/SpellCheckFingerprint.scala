package com.serenity.state.models

import com.serenity.config.*
import com.serenity.lsp.model.Diagnostic

final case class SpellCheckFingerprint(
    contentIdentity: Int,
    contentWeight: Int,
    contentNewlineCount: Int,
    contentLastLineLength: Int,
    usesTextFont: Boolean,
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint],
    config: SpellCheckConfig
)

object SpellCheckFingerprint:

  /** Pure -- takes the dictionaries' on-disk fingerprints as an immutable value rather than reading the filesystem
    * itself. Callers discover `dictionaryFingerprints` once via `SpellCheckConfig.discoverDictionaryFingerprints`
    * inside an explicit `IO.blocking`, then pass the same value into every fingerprint built from that discovery.
    */
  def from(
    buffer: Buffer,
    config: SpellCheckConfig,
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint]
  ): SpellCheckFingerprint =
    SpellCheckFingerprint(
      contentIdentity = System.identityHashCode(buffer.document.content),
      contentWeight = buffer.document.content.weight,
      contentNewlineCount = buffer.document.content.newlineCount,
      contentLastLineLength = buffer.document.content.lastLineLength,
      usesTextFont = buffer.usesTextFont,
      dictionaryFingerprints = dictionaryFingerprints,
      config = config.normalized
    )

final case class SpellCheckCacheEntry(
    fingerprint: SpellCheckFingerprint,
    diagnostics: List[Diagnostic]
)
