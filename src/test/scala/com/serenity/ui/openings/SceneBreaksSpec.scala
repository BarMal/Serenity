package com.serenity.ui.openings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SceneBreaksSpec extends AnyFlatSpec with Matchers:

  "SceneBreaks" should "accept the usual ornaments and rule lines" in
    List(
      "***",
      "* * *",
      "*   *   *",
      "---",
      "- - -",
      "___",
      "_ _ _",
      "#",
      "⁂",
      "❧",
      "~~~",
      "• • •",
      "· · ·",
      "◆ ◆ ◆",
      "*****",
      "—————",
      "  ***  "
    ).foreach(text => withClue(s"'$text': ")(SceneBreaks.isSceneBreak(text) shouldBe true))

  it should "reject text, short rules and mixed characters" in
    List(
      "",
      "  ",
      "*",
      "**",
      "--",
      "- -",
      "__",
      "**bold**",
      "***text",
      "a",
      "x",
      "* - *",
      "###",
      "===",
      "...",
      "1.",
      "# #",
      "- item",
      "---|---"
    ).foreach(text => withClue(s"'$text': ")(SceneBreaks.isSceneBreak(text) shouldBe false))
