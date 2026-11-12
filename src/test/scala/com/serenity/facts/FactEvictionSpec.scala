package com.serenity.facts

import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The eviction planner of the fact store (#2531 F1, the table in §2.4). The bounds are inputs: their values come from
  * F0's measurements, so none is fixed here.
  */
class FactEvictionSpec extends AnyFlatSpec with Matchers:

  private val now = Instant.parse("2026-11-03T12:00:00Z")

  private def hashOf(text: String): ContentHash = ContentHash.of(text.getBytes(StandardCharsets.UTF_8))

  private val bounds = EvictionBounds(globalCapBytes = 1000L, maxAge = Duration.ofDays(30), orphanShare = 0.25)

  private def root(name: String, sizeBytes: Long, daysAgo: Long, open: Boolean = false): RootUsage =
    RootUsage(RootKey(name), sizeBytes, now.minus(Duration.ofDays(daysAgo)), open)

  "FactEviction.rootsToRemove" should "remove nothing while the cache is under its cap and every root is recent" in {
    FactEviction.rootsToRemove(List(root("a", 300, 1), root("b", 300, 2)), bounds, now) shouldBe Nil
  }

  it should "remove roots not opened within the age limit" in {
    FactEviction.rootsToRemove(List(root("a", 10, 1), root("old", 10, 31)), bounds, now) shouldBe List(RootKey("old"))
  }

  it should "remove the least recently opened roots until the cache is under its cap" in {
    val roots = List(root("newest", 400, 1), root("middle", 400, 2), root("oldest", 400, 3))
    FactEviction.rootsToRemove(roots, bounds, now) shouldBe List(RootKey("oldest"))
  }

  it should "never remove an open root, whatever its age or size" in {
    val roots = List(root("open", 5000, 400, open = true), root("other", 10, 1))
    FactEviction.rootsToRemove(roots, bounds, now) shouldBe List(RootKey("other"))
  }

  "FactEviction.orphansToDrop" should "drop nothing when there are no orphans" in {
    FactEviction.orphansToDrop(liveBytes = 100, Map.empty, bounds) shouldBe empty
  }

  it should "keep orphans within their share of the live size, dropping the longest unseen first" in {
    val orphans = Map(
      hashOf("recent") -> OrphanCandidate(60, lastSeenSession = 8),
      hashOf("older")  -> OrphanCandidate(60, lastSeenSession = 5),
      hashOf("oldest") -> OrphanCandidate(60, lastSeenSession = 2)
    )
    // A quarter of 400 bytes is 100: only the most recently seen orphan of 60 bytes fits.
    FactEviction.orphansToDrop(liveBytes = 400, orphans, bounds) shouldBe Set(hashOf("older"), hashOf("oldest"))
  }

  "FactEviction.staleVersions" should "name every stored version of a producer but its current one" in {
    val current = ProducerVersion(2, hashOf("v2"))
    val stored  = Set(ProducerVersion(1, hashOf("v1")), current, ProducerVersion(2, hashOf("other inputs")))
    FactEviction.staleVersions(stored, current) shouldBe Set(
      ProducerVersion(1, hashOf("v1")),
      ProducerVersion(2, hashOf("other inputs"))
    )
  }
