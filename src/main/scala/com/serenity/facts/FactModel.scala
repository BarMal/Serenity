package com.serenity.facts

import java.security.MessageDigest

import scala.collection.immutable.ArraySeq

/** The SHA-256 of a file's bytes: what a fact is keyed by, so the same content shares its facts across branches and
  * renames (#2531 R2). Held as an `ArraySeq` so two hashes compare, and key maps, by value.
  */
opaque type ContentHash = ArraySeq[Byte]

object ContentHash:
  val Length: Int = 32

  def of(bytes: Array[Byte]): ContentHash = ArraySeq.unsafeWrapArray(MessageDigest.getInstance("SHA-256").digest(bytes))

  def fromHex(hex: String): Option[ContentHash] =
    Option.when(hex.length == Length * 2 && hex.forall(Character.digit(_, 16) >= 0))(
      ArraySeq.unsafeWrapArray(hex.grouped(2).map(pair => Integer.parseInt(pair, 16).toByte).toArray)
    )

  private[facts] def fromBytes(bytes: ArraySeq[Byte]): Option[ContentHash] = Option.when(bytes.length == Length)(bytes)

  extension (hash: ContentHash)
    def hex: String                   = hash.map(byte => f"${byte & 0xff}%02x").mkString
    def bytes: ArraySeq[Byte]         = hash
    private[facts] def firstByte: Int = hash(0) & 0xff

  given Ordering[ContentHash] = Ordering.by(hex)

/** A producer of facts, such as `treesitter.tags`. It names no language: languages enter through its version. */
opaque type ProducerId = String

object ProducerId:
  def apply(value: String): ProducerId = value

  extension (id: ProducerId) def value: String = id

/** Which facts a producer writes: `codec` changes when their encoding does, `inputs` when the data they are derived
  * with does (grammar binary, queries, the pack sections read), so a changed grammar invalidates only its own facts.
  */
final case class ProducerVersion(codec: Int, inputs: ContentHash)

final case class FactKey(content: ContentHash, producer: ProducerId, version: ProducerVersion)

/** A stored fact's encoded value, and the session it was last found useful in, which orders orphan eviction. */
final case class FactEntry(lastSeenSession: Long, value: ArraySeq[Byte])
