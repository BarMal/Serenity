package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChapterGhostCacheRetentionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val base = AppState.initial.persisted.buffers.values.head

  private def notedBuffer(id: Int): Buffer =
    base.copy(
      id = BufferId(id),
      annotations = Annotations(notes = Map(NoteKey.Keyword("note") -> Notes(BufferId(900))))
    )

  private def cacheHolding(ids: Int*): ChapterGhostCache =
    val cache   = ChapterGhostCache()
    val buffers = ids.map(notedBuffer)
    buffers.foreach(buffer => cache.ghostsFor(buffer, buffers.map(b => b.id -> b).toMap))
    cache

  "retainOnly" should "drop the entries of buffers that are no longer live and keep the others" in {
    val cache = cacheHolding(1, 2, 3)
    cache.entryCount shouldBe 3

    cache.retainOnly(_.value != 2)

    cache.entryCount shouldBe 2
  }

  it should "empty the cache when no buffer is live" in {
    val cache = cacheHolding(1, 2)

    cache.retainOnly(_ => false)

    cache.entryCount shouldBe 0
  }

  it should "leave the cache as it is when every buffer is live" in {
    val cache = cacheHolding(1, 2)

    cache.retainOnly(_ => true)

    cache.entryCount shouldBe 2
  }
