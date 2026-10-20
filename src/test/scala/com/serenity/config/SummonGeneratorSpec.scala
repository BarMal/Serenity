package com.serenity.config

import com.serenity.keystroke.Modifier
import com.serenity.testkit.ConfigGenerators
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SummonGeneratorSpec extends AnyFlatSpec with Matchers:

  private val platforms = List("linux", "Mac OS X", "Windows 11")

  private def conflictsOn(osName: String, summon: SummonConfig) =
    summon.settled(HotkeyConfig.forOs(osName), FocusedKeymapConfig())._2

  "Cmd+Shift+Z" should "be redo on macOS, so a summon letter there is given up to it" in {
    val summon = SummonConfig(
      modifiers = Set(Modifier.Meta, Modifier.Shift),
      letters = Map(
        ChromeElement.LineNumbers -> 'q',
        ChromeElement.Scrollbar   -> 'y',
        ChromeElement.PaneHeaders -> 'z'
      )
    )

    val (settled, conflicts) = summon.settled(HotkeyConfig.forOs("Mac OS X"), FocusedKeymapConfig())

    settled.letterOf(ChromeElement.PaneHeaders) shouldBe None
    conflicts.map(_.keptBy) shouldBe List("redo")
    conflictsOn("linux", summon) shouldBe empty
  }

  "The summon configuration generator" should "make chords that no platform's hotkeys take, or a save would not come back" in {
    val params = Gen.Parameters.default
    val clashing = (0L until 2000L).flatMap { n =>
      ConfigGenerators.genSummonConfig.apply(params, Seed(n)).toList.flatMap { summon =>
        platforms.collect { case osName if conflictsOn(osName, summon).nonEmpty => s"$osName: $summon" }
      }
    }

    clashing shouldBe empty
  }
