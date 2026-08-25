package com.serenity.testkit

import cats.kernel.laws.discipline.MonoidTests
import cats.syntax.all.*
import cats.{Eq, Monoid}
import com.serenity.state.models.{BufferId, Damage, PaneId, SurfaceId}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** The laws are what let a queue of `Damage` values be folded in any grouping and still land on the same repaint
  * decision -- `combine` re-groups every leaf by its target on every call, so the result depends only on the multiset
  * of facts involved, never on which pairs were folded first.
  */
class DamageLawSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Matchers:

  private given Eq[Damage] = Eq.fromUniversalEquals

  private val leaves: Gen[Damage] =
    Gen.oneOf(
      Gen
        .choose(0, 2)
        .flatMap(id => Gen.someOf(0, 1, 2, 3).map(rows => Damage.BufferRows(BufferId(id), rows.toSet): Damage)),
      for
        id   <- Gen.choose(0, 2)
        row  <- Gen.choose(0, 3)
        from <- Gen.choose(0, 10)
        to   <- Gen.option(Gen.choose(from, from + 10))
      yield Damage.BufferCells(BufferId(id), row, from, to): Damage,
      Gen.choose(0, 2).map(id => Damage.PaneChrome(PaneId(id)): Damage),
      Gen.choose(0, 2).map(id => Damage.Surface(SurfaceId(s"surface-$id")): Damage),
      Gen.const(Damage.Chrome: Damage),
      Gen.const(Damage.Everything: Damage)
    )

  /** Built by folding leaves through the `Monoid` under test, so every generated value is already in the normal form
    * `combine` would itself produce -- generating a raw, un-normalized `Combined` directly would fail the identity law
    * on that value alone, since normalizing it would not round-trip to the same (non-canonical) shape.
    */
  private given Arbitrary[Damage] =
    Arbitrary(Gen.listOf(leaves).map(_.foldLeft(Monoid[Damage].empty)(_ |+| _)))

  checkAll("Monoid[Damage]", MonoidTests[Damage].monoid)

  test("Nothing is dropped rather than accumulated") {
    val rows: Damage = Damage.BufferRows(BufferId(0), Set(1, 2))

    ((Damage.Nothing: Damage) |+| rows) shouldBe rows
    (rows |+| (Damage.Nothing: Damage)) shouldBe rows
    ((Damage.Nothing: Damage) |+| (Damage.Nothing: Damage)) shouldBe Damage.Nothing
  }

  test("BufferRows for the same buffer unions rather than nests") {
    val first: Damage  = Damage.BufferRows(BufferId(0), Set(1, 2))
    val second: Damage = Damage.BufferRows(BufferId(0), Set(2, 3))

    (first |+| second) shouldBe Damage.BufferRows(BufferId(0), Set(1, 2, 3))
  }

  test("BufferCells on the same row merges to a spanning range") {
    val first: Damage  = Damage.BufferCells(BufferId(0), row = 4, fromColumn = 2, toColumn = Some(6))
    val second: Damage = Damage.BufferCells(BufferId(0), row = 4, fromColumn = 5, toColumn = Some(9))

    (first |+| second) shouldBe Damage.BufferCells(BufferId(0), row = 4, fromColumn = 2, toColumn = Some(9))
  }

  test("an unbounded BufferCells span stays unbounded once merged") {
    val bounded: Damage   = Damage.BufferCells(BufferId(0), row = 4, fromColumn = 2, toColumn = Some(6))
    val unbounded: Damage = Damage.BufferCells(BufferId(0), row = 4, fromColumn = 5, toColumn = None)

    (bounded |+| unbounded) shouldBe Damage.BufferCells(BufferId(0), row = 4, fromColumn = 2, toColumn = None)
  }

  test("BufferCells already covered by a whole-row report is dropped") {
    val rows: Damage  = Damage.BufferRows(BufferId(0), Set(4))
    val cells: Damage = Damage.BufferCells(BufferId(0), row = 4, fromColumn = 2, toColumn = Some(6))

    (rows |+| cells) shouldBe rows
  }

  test("Everything absorbs any other damage") {
    val rows: Damage = Damage.BufferRows(BufferId(0), Set(1))

    ((Damage.Everything: Damage) |+| rows) shouldBe Damage.Everything
    (rows |+| (Damage.Everything: Damage)) shouldBe Damage.Everything
  }

  test("facts about different targets combine instead of collapsing into one") {
    val rows: Damage   = Damage.BufferRows(BufferId(0), Set(1))
    val pane: Damage   = Damage.PaneChrome(PaneId(0))
    val chrome: Damage = Damage.Chrome

    val combined = rows |+| pane |+| chrome

    combined shouldBe Damage.Combined(Set(rows, pane, chrome))
  }

  test("combining is insensitive to grouping order") {
    val a: Damage = Damage.BufferRows(BufferId(0), Set(1))
    val b: Damage = Damage.PaneChrome(PaneId(0))
    val c: Damage = Damage.BufferRows(BufferId(0), Set(2))
    val d: Damage = Damage.Surface(SurfaceId("find"))

    ((a |+| b) |+| (c |+| d)) shouldBe (a |+| (b |+| c) |+| d)
  }

  test("coarsenToRows reads back BufferRows and BufferCells for the requested buffer only") {
    val bufferId = BufferId(0)
    val damage: Damage =
      (Damage.BufferRows(bufferId, Set(1, 2)): Damage) |+|
        (Damage.BufferCells(BufferId(1), row = 9, fromColumn = 0, toColumn = None): Damage) |+|
        (Damage.PaneChrome(PaneId(0)): Damage)

    Damage.coarsenToRows(bufferId, damage) shouldBe Set(1, 2)
  }

  test("isEverything is true only once Everything has been combined in") {
    val rows: Damage = Damage.BufferRows(BufferId(0), Set(1))

    Damage.isEverything(rows) shouldBe false
    Damage.isEverything(rows |+| (Damage.Everything: Damage)) shouldBe true
  }
