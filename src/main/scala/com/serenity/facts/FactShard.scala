package com.serenity.facts

import java.nio.charset.StandardCharsets
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.util.zip.CRC32C

import scala.collection.immutable.ArraySeq

enum ShardRefusal:
  /** The bytes are not a whole, intact shard: damaged, truncated, or from another layout. */
  case Corrupt(reason: String)

  /** An intact shard of another producer or another version of this one. */
  case WrongProducer

/** Every fact of one producer version whose content hash starts with one byte (#2531 F1). Rewritten whole when any of
  * its facts change, so it never needs compacting.
  *
  * Layout: magic, layout version, producer id, codec, inputs, entry count, then one index row per entry (hash, last
  * seen, value length), then a CRC32C of all of that; then each value followed by its own CRC32C. Nothing may follow.
  */
final case class FactShard(producer: ProducerId, version: ProducerVersion, entries: Map[ContentHash, FactEntry])

object FactShard:

  val LayoutVersion: Int = 1

  private val Magic         = "SRFS".getBytes(StandardCharsets.US_ASCII)
  private val IndexRowBytes = ContentHash.Length + java.lang.Long.BYTES + Integer.BYTES

  def shardIndexOf(hash: ContentHash): Int = hash.firstByte

  def encode(shard: FactShard): Array[Byte] =
    val producer = shard.producer.value.getBytes(StandardCharsets.UTF_8)
    val rows     = shard.entries.toVector.sortBy(_._1)
    val headerBytes =
      Magic.length + Integer.BYTES * 3 + producer.length + ContentHash.Length + Integer.BYTES + rows.size * IndexRowBytes
    val valueBytes = rows.map((_, entry) => entry.value.length + Integer.BYTES).sum
    val buffer     = ByteBuffer.allocate(headerBytes + Integer.BYTES + valueBytes)
    buffer.put(Magic).putInt(LayoutVersion).putInt(producer.length).put(producer)
    buffer.putInt(shard.version.codec).put(shard.version.inputs.bytes.toArray).putInt(rows.size)
    rows.foreach((hash, entry) =>
      buffer.put(hash.bytes.toArray).putLong(entry.lastSeenSession).putInt(entry.value.length)
    )
    buffer.putInt(crcOf(buffer.array, 0, headerBytes))
    rows.foreach { (_, entry) =>
      val value = entry.value.toArray
      buffer.put(value).putInt(crcOf(value, 0, value.length))
    }
    buffer.array

  /** The shard `bytes` hold, if they are an intact shard of exactly this producer version. Total: any damage is a
    * `Left`, never an exception, so a bad cache file only costs a recompute.
    */
  def decode(bytes: Array[Byte], producer: ProducerId, version: ProducerVersion): Either[ShardRefusal, FactShard] =
    val buffer = ByteBuffer.wrap(bytes)
    try
      for
        _  <- check(bytes.length >= Magic.length && bytes.take(Magic.length).sameElements(Magic), "not a shard")
        _  <- check({ buffer.position(Magic.length); buffer.getInt == LayoutVersion }, "another layout")
        id <- utf8(buffer)
        codec = buffer.getInt
        inputs <- hash(buffer)
        count = buffer.getInt
        _    <- check(count >= 0 && count.toLong * IndexRowBytes <= buffer.remaining, "impossible entry count")
        rows <- index(buffer, count)
        _    <- check(buffer.getInt == crcOf(bytes, 0, buffer.position - Integer.BYTES), "index checksum")
        _ <- Either.cond(
          id == producer.value && ProducerVersion(codec, inputs) == version,
          (),
          ShardRefusal.WrongProducer
        )
        entries <- values(buffer, rows)
        _       <- check(!buffer.hasRemaining, "trailing bytes")
      yield FactShard(producer, version, entries)
    catch case _: BufferUnderflowException | _: IllegalArgumentException => Left(ShardRefusal.Corrupt("truncated"))

  final private case class IndexRow(hash: ContentHash, lastSeenSession: Long, length: Int)

  private def check(condition: => Boolean, reason: String): Either[ShardRefusal, Unit] =
    Either.cond(condition, (), ShardRefusal.Corrupt(reason))

  private def utf8(buffer: ByteBuffer): Either[ShardRefusal, String] =
    val length = buffer.getInt
    check(length >= 0 && length <= buffer.remaining, "producer id length").map { _ =>
      val bytes = new Array[Byte](length)
      buffer.get(bytes)
      String(bytes, StandardCharsets.UTF_8)
    }

  private def hash(buffer: ByteBuffer): Either[ShardRefusal, ContentHash] =
    val bytes = new Array[Byte](ContentHash.Length)
    buffer.get(bytes)
    ContentHash.fromBytes(ArraySeq.unsafeWrapArray(bytes)).toRight(ShardRefusal.Corrupt("hash"))

  private def index(buffer: ByteBuffer, count: Int): Either[ShardRefusal, Vector[IndexRow]] =
    (0 until count).foldLeft[Either[ShardRefusal, Vector[IndexRow]]](Right(Vector.empty)) { (rows, _) =>
      for
        done   <- rows
        digest <- hash(buffer)
        seen   = buffer.getLong
        length = buffer.getInt
        _ <- check(length >= 0, "value length")
      yield done :+ IndexRow(digest, seen, length)
    }

  private def values(buffer: ByteBuffer, rows: Vector[IndexRow]): Either[ShardRefusal, Map[ContentHash, FactEntry]] =
    rows.foldLeft[Either[ShardRefusal, Map[ContentHash, FactEntry]]](Right(Map.empty)) { (entries, row) =>
      for
        done <- entries
        _    <- check(row.length.toLong + Integer.BYTES <= buffer.remaining, "value runs past the end")
        value =
          val bytes = new Array[Byte](row.length); buffer.get(bytes); bytes
        _ <- check(buffer.getInt == crcOf(value, 0, value.length), "value checksum")
        _ <- check(!done.contains(row.hash), "duplicate hash")
      yield done.updated(row.hash, FactEntry(row.lastSeenSession, ArraySeq.unsafeWrapArray(value)))
    }

  private def crcOf(bytes: Array[Byte], offset: Int, length: Int): Int =
    val crc = CRC32C()
    crc.update(bytes, offset, length)
    crc.getValue.toInt
