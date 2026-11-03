package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PictureSlotSpec extends AnyFlatSpec with Matchers:

  private def slot(closed: AtomicReference[List[String]]) =
    new PictureSlot[String](name =>
      val _ = closed.updateAndGet(name :: _)
    )

  "PictureSlot" should "draw nothing before anything is published" in {
    val drawn = new AtomicReference(List.empty[String])
    slot(new AtomicReference(Nil)).drawLatest(name =>
      val _ = drawn.updateAndGet(name :: _)
    ) shouldBe false
    drawn.get shouldBe Nil
  }

  it should "draw the latest picture" in {
    val drawn = new AtomicReference(List.empty[String])
    val s     = slot(new AtomicReference(Nil))
    s.publish("a")
    s.publish("b")
    s.drawLatest(name =>
      val _ = drawn.updateAndGet(name :: _)
    ) shouldBe true
    drawn.get shouldBe List("b")
  }

  it should "close a replaced picture only after the next draw, never while it might be drawn" in {
    val closed = new AtomicReference(List.empty[String])
    val s      = slot(closed)
    s.publish("a")
    s.drawLatest(_ => ())
    s.publish("b")
    closed.get shouldBe Nil
    s.drawLatest(_ => ())
    closed.get shouldBe List("a")
  }

  it should "close the picture it holds, and anything still waiting, when closed" in {
    val closed = new AtomicReference(List.empty[String])
    val s      = slot(closed)
    s.publish("a")
    s.drawLatest(_ => ())
    s.publish("b")
    s.closeAll()
    closed.get.toSet shouldBe Set("a", "b")
    s.drawLatest(_ => ()) shouldBe false
  }

  it should "count what it has closed" in {
    val s = slot(new AtomicReference(Nil))
    s.publish("a")
    s.publish("b")
    s.drawLatest(_ => ())
    s.closeAll()
    s.closedCount shouldBe 2
  }
