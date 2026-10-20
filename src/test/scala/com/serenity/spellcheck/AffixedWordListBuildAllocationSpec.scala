package com.serenity.spellcheck

import com.serenity.perf.SettledAllocation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AffixedWordListBuildAllocationSpec extends AnyFlatSpec with Matchers:

  private val entryCount = 1000

  private val entries: Vector[HunspellEntry] =
    Vector.tabulate(entryCount)(index => HunspellEntry(s"Word$index", Set(s"F${index % 7}", "S"))) ++ Vector(
      HunspellEntry("word5", Set("X"))
    )

  private def build() = AffixedWordList.build(entries.iterator, HunspellAffixRules.empty)

  "AffixedWordList.build" should "keep homonyms apart, in dictionary order, with equal flag sets shared" in {
    val stems = build().stems
    stems.size shouldBe entryCount
    stems("word5") shouldBe List(Set("F5", "S"), Set("X"))
    (stems("word0").head: AnyRef) should be theSameInstanceAs stems("word7").head
  }

  it should "allocate little beyond the stem table it returns" in {
    SettledAllocation.perCall(() => build(), () => build()) match
      case Some((bytes, _)) =>
        val perEntry = bytes.toDouble / entries.length
        withClue(s"$perEntry bytes per entry: ")(perEntry should be < 450.0)
      case None => info("per-thread allocation counter unsupported -- skipping")
  }
