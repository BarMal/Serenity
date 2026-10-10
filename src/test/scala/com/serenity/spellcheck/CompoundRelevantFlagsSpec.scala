package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompoundRelevantFlagsSpec extends AnyFlatSpec with Matchers:

  private val rules = HunspellAffixRules.empty.copy(
    compoundRules = List("AB*", "(12)(34)?"),
    compoundFlag = Some("C")
  )

  "compoundRelevantFlags" should "collect the flags every compound mechanism consults" in {
    rules.compoundRelevantFlags shouldBe Set("A", "B", "12", "34", "C")
  }

  it should "be derived once per rule set, because the loader asks for it once per dictionary entry" in {
    (rules.compoundRelevantFlags: AnyRef) should be theSameInstanceAs rules.compoundRelevantFlags
  }
