package com.serenity.ui.layout

import scala.collection.mutable

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A key that grows from one the store has measured starts with that key's one-row lines measured (#2447 B4): a line
  * that fitted a row at a narrower width still fits one at a wider width, so its count needs no wrap. The store is told
  * which key may seed which; here a key is a width and a narrower one seeds a wider one.
  */
class VisualLineIndexStoreSeedingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val lineCount = 10

  private def text(edit: Option[(Int, String)] = None): Rope = Rope(
    (0 until lineCount)
      .map(line => edit.collect { case (at, replacement) if at == line => replacement }.getOrElse(s"line $line"))
      .mkString("\n")
  )

  private def newStore = new VisualLineIndexStore[Int](8, seedsFrom = (narrower, wider) => narrower < wider)

  final private class Measuring(rowsAt: Int => Int):
    val measured = mutable.TreeSet.empty[Int]

    def apply(line: Int): Int =
      measured += line
      rowsAt(line)

  /** Even lines fit one row at any width counted here; odd lines take `oddRows` rows. */
  private def evenLinesFit(oddRows: Int)(line: Int): Int = if line % 2 == 0 then 1 else oddRows

  private def count(
    store: VisualLineIndexStore[Int],
    width: Int,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef = VisualLineIndexStore.Unstamped
  ): Int = store.counts(width, content, measure, stamp).rowsBetween(0, content.lineCount)

  private val odd = Set(1, 3, 5, 7, 9)

  "A key wider than one the store has measured" should "measure only the lines that were not one row" in {
    val store = newStore
    val _     = count(store, 100, text(), evenLinesFit(oddRows = 3))
    val wide  = Measuring(evenLinesFit(oddRows = 2))

    val total = count(store, 200, text(), wide(_))

    wide.measured.toSet shouldBe odd
    total shouldBe 5 * 1 + 5 * 2
  }

  it should "count every line as a cold key does" in {
    val seeded = newStore
    val _      = count(seeded, 100, text(), evenLinesFit(oddRows = 3))
    val cold   = newStore

    val rowsAt200 = evenLinesFit(oddRows = 2)
    (0 until lineCount).map(line => seeded.counts(200, text(), rowsAt200).rowsIn(line)) shouldBe
      (0 until lineCount).map(line => cold.counts(200, text(), rowsAt200).rowsIn(line))
  }

  it should "seed from the widest narrower key, which knows the most one-row lines" in {
    val store = newStore
    val _     = count(store, 100, text(), line => if line < 3 then 1 else 3)
    val _     = count(store, 150, text(), line => if line < 5 then 1 else 3)
    val wide  = Measuring(_ => 2)

    count(store, 200, text(), wide(_))

    wide.measured.toSet shouldBe (5 until lineCount).toSet
  }

  "A key narrower than one the store has measured" should "measure every line" in {
    val store = newStore
    val _     = count(store, 200, text(), evenLinesFit(oddRows = 2))
    val narrow = Measuring(evenLinesFit(oddRows = 3))

    count(store, 100, text(), narrow(_))

    narrow.measured.toSet shouldBe (0 until lineCount).toSet
  }

  "A wider key asked about edited content" should "measure the edited line besides the lines that were not one row" in {
    val store = newStore
    val _     = count(store, 100, text(), evenLinesFit(oddRows = 3))
    val wide  = Measuring(evenLinesFit(oddRows = 2))

    count(store, 200, text(Some(4 -> "an edited line")), wide(_))

    wide.measured.toSet shouldBe odd + 4
  }

  "A wider key asked about the same lines under a different stamp" should "seed nothing" in {
    val store = newStore
    val _     = count(store, 100, text(), evenLinesFit(oddRows = 3), stamp = new Object)
    val wide  = Measuring(evenLinesFit(oddRows = 2))

    count(store, 200, text(), wide(_), stamp = new Object)

    wide.measured.toSet shouldBe (0 until lineCount).toSet
  }

  "A store told nothing about which key seeds which" should "seed nothing" in {
    val store = new VisualLineIndexStore[Int](8)
    val _     = count(store, 100, text(), evenLinesFit(oddRows = 3))
    val wide  = Measuring(evenLinesFit(oddRows = 2))

    count(store, 200, text(), wide(_))

    wide.measured.toSet shouldBe (0 until lineCount).toSet
  }

  "The index a key seeded from" should "keep its own measurements" in {
    val store = newStore
    val _     = count(store, 100, text(), evenLinesFit(oddRows = 3))
    val _     = count(store, 200, text(), evenLinesFit(oddRows = 2))
    val again = Measuring(evenLinesFit(oddRows = 3))

    count(store, 100, text(), again(_))

    again.measured shouldBe empty
  }
