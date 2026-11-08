package com.serenity.facts

import java.nio.charset.StandardCharsets

import scala.collection.immutable.ArraySeq

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The on-disk shard of the fact store (#2531 F1): every fact of one producer version whose content hash starts with
  * one byte. A shard that cannot be trusted must decode to a `Left`, never throw and never return wrong facts.
  */
class FactShardSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private val producer = ProducerId("treesitter.tags")
  private val version =
    ProducerVersion(codec = 1, inputs = ContentHash.of("grammar+queries".getBytes(StandardCharsets.UTF_8)))

  private def hashOf(text: String): ContentHash = ContentHash.of(text.getBytes(StandardCharsets.UTF_8))

  private def shardOf(entries: (String, String, Long)*): FactShard =
    FactShard(
      producer,
      version,
      entries
        .map((content, value, seen) =>
          hashOf(content) -> FactEntry(seen, ArraySeq.unsafeWrapArray(value.getBytes(StandardCharsets.UTF_8)))
        )
        .toMap
    )

  private val sample = shardOf(("a", "fact of a", 3L), ("b", "fact of b", 1L), ("c", "", 7L))

  "ContentHash" should "be the SHA-256 of the bytes, shown as lowercase hex" in {
    hashOf("abc").hex shouldBe "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  }

  it should "read back from its hex and refuse text that is not 32 bytes of hex" in {
    ContentHash.fromHex(hashOf("abc").hex) shouldBe Some(hashOf("abc"))
    ContentHash.fromHex("abcd") shouldBe None
    ContentHash.fromHex("z" * 64) shouldBe None
  }

  it should "compare by value" in {
    hashOf("x") shouldBe hashOf("x")
    hashOf("x") should not be hashOf("y")
  }

  "FactShard" should "decode exactly what it encoded" in {
    FactShard.decode(FactShard.encode(sample), producer, version) shouldBe Right(sample)
  }

  it should "round-trip an empty shard" in {
    val empty = FactShard(producer, version, Map.empty)
    FactShard.decode(FactShard.encode(empty), producer, version) shouldBe Right(empty)
  }

  private val entries: Gen[Map[ContentHash, FactEntry]] =
    Gen.mapOf(
      Gen.zip(
        Gen.alphaNumStr.map(hashOf),
        Gen
          .zip(
            Gen.posNum[Long],
            Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bytes => ArraySeq.from(bytes))
          )
          .map(FactEntry(_, _))
      )
    )

  it should "round-trip any entries" in
    forAll(entries) { generated =>
      val shard = FactShard(producer, version, generated)
      FactShard.decode(FactShard.encode(shard), producer, version) shouldBe Right(shard)
    }

  it should "refuse a shard with any single byte changed" in {
    val bytes = FactShard.encode(sample)
    bytes.indices.foreach { index =>
      val damaged = bytes.updated(index, (bytes(index) ^ 0x5a).toByte)
      withClue(s"byte $index: ")(FactShard.decode(damaged, producer, version).isLeft shouldBe true)
    }
  }

  it should "refuse every truncation of a shard" in {
    val bytes = FactShard.encode(sample)
    (0 until bytes.length).foreach { length =>
      withClue(s"first $length bytes: ")(FactShard.decode(bytes.take(length), producer, version).isLeft shouldBe true)
    }
  }

  it should "refuse a shard written by another producer or another version of this one" in {
    val bytes = FactShard.encode(sample)
    FactShard.decode(bytes, ProducerId("other"), version) shouldBe Left(ShardRefusal.WrongProducer)
    FactShard.decode(bytes, producer, version.copy(codec = 2)) shouldBe Left(ShardRefusal.WrongProducer)
  }

  it should "never throw on arbitrary bytes" in
    forAll(Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue))) { bytes =>
      noException should be thrownBy FactShard.decode(bytes.toArray, producer, version)
    }

  it should "place a hash in the shard named by its first byte" in {
    val hash = hashOf("abc")
    FactShard.shardIndexOf(hash) shouldBe 0xba
  }
