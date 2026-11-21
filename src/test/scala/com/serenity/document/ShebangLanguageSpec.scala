package com.serenity.document

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.FileManager
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A script with no extension is code, and says so on its first line. */
class ShebangLanguageSpec extends AnyFlatSpec with Matchers:

  "A shebang line" should "name the language of the interpreters the editor knows" in {
    val cases = List(
      "#!/usr/bin/python"                -> LanguageId.Python,
      "#!/usr/bin/env python3"           -> LanguageId.Python,
      "#!/usr/bin/env python3.12 -u"     -> LanguageId.Python,
      "#!/usr/bin/env -S python3 -u"     -> LanguageId.Python,
      "#! /usr/bin/ruby"                 -> LanguageId.Ruby,
      "#!/usr/bin/env lua"               -> LanguageId.Lua,
      "#!/usr/bin/luajit"                -> LanguageId.Lua,
      "#!/usr/bin/env node"              -> LanguageId.JavaScript,
      "#!/usr/bin/env deno run --allow-read" -> LanguageId.JavaScript
    )

    cases.foreach((line, language) => withClue(s"$line: ")(ShebangLanguage.of(line) shouldBe Some(language)))
  }

  it should "say nothing for an interpreter with no language here, or for text that is not a shebang" in {
    List("#!/bin/sh", "#!/usr/bin/env bash", "# python3", "python3", "", "#!", "#!/usr/bin/env").foreach { line =>
      withClue(s"'$line': ")(ShebangLanguage.of(line) shouldBe None)
    }
  }

  "Opening a file with no extension" should "take its language from the shebang, and leave one with no shebang plain" in {
    val script = TestTemp.file("serenity-shebang-script", "")
    val notes  = TestTemp.file("serenity-shebang-notes", "")
    Files.writeString(script, "#!/usr/bin/env python3\nprint('hi')\n")
    Files.writeString(notes, "Remember the milk.\n")
    val manager = new FileManager()

    manager.loadFile(script, BufferId(1)).unsafeRunSync().document.language shouldBe Some(LanguageId.Python)
    manager.loadFile(notes, BufferId(2)).unsafeRunSync().document.language shouldBe None
  }

  it should "still prefer the extension to the shebang" in {
    val markdown = TestTemp.file("serenity-shebang-ext", ".md")
    Files.writeString(markdown, "#!/usr/bin/env python3\nnot code\n")

    new FileManager().loadFile(markdown, BufferId(3)).unsafeRunSync().document.language shouldBe
      Some(LanguageId.Markdown)
  }
