package com.serenity.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextCountsSegmentationScopeSpec extends AnyFlatSpec with Matchers:

  private def onAnotherThread(work: => Unit): Unit =
    val thread = new Thread(() => work)
    thread.start()
    thread.join()

  "countSegmentations" should "count the segmentations the calling thread performs" in {
    val (words, runs) = TextCounts.countSegmentations(TextCounts.of("hello world").words)

    words shouldBe 2
    runs shouldBe 1L
  }

  it should "ignore segmentation done concurrently by other threads" in {
    val (_, runs) = TextCounts.countSegmentations(
      onAnotherThread((1 to 1000).foreach(_ => TextCounts.of("hello world")))
    )

    runs shouldBe 0L
  }

  it should "measure each scope from its own start" in {
    TextCounts.of("before")

    val (_, runs) = TextCounts.countSegmentations((1 to 3).foreach(_ => TextCounts.of("inside")))

    runs shouldBe 3L
  }
