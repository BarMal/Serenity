package com.serenity.state.models

import com.serenity.config.*
import com.serenity.lsp.model.Diagnostic
import com.serenity.rope.Rope

/** The very rope a check was made of. Equal only to itself, which is what "unchanged" means for an immutable rope: an
  * identity hash code can be shared by two ropes, so it cannot stand in for the rope, and comparing the text would cost
  * the whole document.
  */
final class ContentIdentity(val content: Rope):

  override def equals(other: Any): Boolean =
    other match
      case that: ContentIdentity => that.content eq content
      case _                     => false

  override def hashCode: Int = System.identityHashCode(content)

  override def toString: String = s"ContentIdentity(${hashCode.toHexString})"

final case class SpellCheckFingerprint(
    contentIdentity: ContentIdentity,
    contentWeight: Int,
    contentNewlineCount: Int,
    contentLastLineLength: Int,
    usesTextFont: Boolean,
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint],
    config: SpellCheckConfig
)

object SpellCheckFingerprint:

  extension (fingerprint: SpellCheckFingerprint)

    /** Whether `other` differs from this only in the content it was taken of. */
    def sameDictionaryAndConfig(other: SpellCheckFingerprint): Boolean =
      fingerprint.usesTextFont == other.usesTextFont &&
        fingerprint.dictionaryFingerprints == other.dictionaryFingerprints &&
        fingerprint.config == other.config

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
      contentIdentity = new ContentIdentity(buffer.document.content),
      contentWeight = buffer.document.content.weight,
      contentNewlineCount = buffer.document.content.newlineCount,
      contentLastLineLength = buffer.document.content.lastLineLength,
      usesTextFont = buffer.usesTextFont,
      dictionaryFingerprints = dictionaryFingerprints,
      config = config.normalized
    )

final case class SpellCheckCacheEntry(
    fingerprint: SpellCheckFingerprint,
    diagnostics: List[Diagnostic],
    analysis: Option[SpellCheckAnalysis] = None,
    contentVersion: Option[Long] = None
)
