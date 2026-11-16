package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextMeasurer
import com.serenity.ui.renderer.BackendCaches
import com.serenity.ui.skia.SkiaTextMeasurer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class RenderCachesMeasurerSpec extends AnyFlatSpec with Matchers:

  "RenderCaches" should "have no measurer unless its creator names how to make one" in {
    TestMeasure.renderCaches().measurer shouldBe None
  }

  it should "make the measurer from its own backend caches, once, and only when it is asked for" in {
    val made = new AtomicInteger(0)
    val seen = new java.util.concurrent.atomic.AtomicReference[Option[BackendCaches]](None)
    val caches = RenderCaches.create(measurerFor = owner =>
      made.incrementAndGet()
      seen.set(Some(owner))
      Some(SkiaTextMeasurer.of(owner)))
    made.get shouldBe 0
    val first: Option[TextMeasurer] = caches.measurer
    first should not be empty
    caches.measurer should be theSameInstanceAs first
    made.get shouldBe 1
    seen.get().exists(_ eq caches.backendCaches) shouldBe true
  }

  it should "carry the measurer on the caches layout measures through, made only when one of them is asked" in {
    val made = new AtomicInteger(0)
    val caches = RenderCaches.create(measurerFor = owner =>
      made.incrementAndGet()
      Some(SkiaTextMeasurer.of(owner)))
    made.get shouldBe 0
    val fromShaped = caches.wrappedLines.layoutMemos.measurer
    made.get shouldBe 1
    fromShaped should not be empty
    caches.measurer shouldBe fromShaped
    made.get shouldBe 1
  }

  "LayoutMemos made on their own" should "have no measurer" in {
    TestMeasure.layoutMemos().measurer shouldBe None
  }
