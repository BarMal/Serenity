package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListIdOpsSpec extends AnyFlatSpec with Matchers:

  private case class Item(id: Int, value: String)

  "replacedWhere" should "replace the matching item in place, keeping its position" in {
    val items = List(Item(1, "a"), Item(2, "b"), Item(3, "c"))

    items.replacedWhere(_.id == 2)(i => i.copy(value = "updated")) shouldBe
      List(Item(1, "a"), Item(2, "updated"), Item(3, "c"))
  }

  it should "be a no-op when nothing matches" in {
    val items = List(Item(1, "a"), Item(2, "b"))

    items.replacedWhere(_.id == 99)(i => i.copy(value = "updated")) shouldBe items
  }

  it should "replace every matching item when more than one matches" in {
    val items = List(Item(1, "a"), Item(1, "b"), Item(2, "c"))

    items.replacedWhere(_.id == 1)(i => i.copy(value = "x")) shouldBe
      List(Item(1, "x"), Item(1, "x"), Item(2, "c"))
  }

  "movedToEndWhere" should "drop the matching item and append the replacement" in {
    val items       = List(Item(1, "a"), Item(2, "b"), Item(3, "c"))
    val replacement = Item(2, "updated")

    items.movedToEndWhere(_.id == 2)(replacement) shouldBe
      List(Item(1, "a"), Item(3, "c"), Item(2, "updated"))
  }

  it should "append the replacement even when nothing matched" in {
    val items       = List(Item(1, "a"), Item(2, "b"))
    val replacement = Item(3, "c")

    items.movedToEndWhere(_.id == 99)(replacement) shouldBe
      List(Item(1, "a"), Item(2, "b"), Item(3, "c"))
  }
