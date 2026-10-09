package com.serenity.release

import java.nio.file.{Files, Path}

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChangelogNotesSpec extends AnyFlatSpec with Matchers:

  private val changelog =
    """# Changelog
      |
      |## [Unreleased]
      |
      |- Pending change.
      |
      |## 1.2.0 — 2026-10-01
      |
      |- Shipped change.
      |""".stripMargin

  private def withChangelog(test: (String, String) => Unit): Unit =
    val dir  = TestTemp.directory("changelog-notes")
    val file = dir.resolve("CHANGELOG.md")
    Files.writeString(file, changelog)
    test(file.toString, dir.resolve("notes.md").toString)

  "ChangelogNotes.write" should "write a released version's section to the output file" in withChangelog {
    (file, out) =>
      ChangelogNotes.write(List(file, "1.2.0", out)) shouldBe Right(Path.of(out))
      Files.readString(Path.of(out)) should include("Shipped change.")
  }

  it should "write the unreleased section when asked for it" in withChangelog { (file, out) =>
    ChangelogNotes.write(List(file, "--unreleased", out)).isRight shouldBe true
    Files.readString(Path.of(out)) should include("Pending change.")
  }

  it should "report a missing version and leave no output file" in withChangelog { (file, out) =>
    ChangelogNotes.write(List(file, "9.9.9", out)).isLeft shouldBe true
    Files.exists(Path.of(out)) shouldBe false
  }

  it should "report an unreadable changelog" in withChangelog { (_, out) =>
    ChangelogNotes.write(List("/no/such/CHANGELOG.md", "1.2.0", out)).left.map(_.headOption) should matchPattern {
      case Left(Some(message: String)) if message.startsWith("cannot read") =>
    }
  }

  it should "report usage when the output file is missing" in withChangelog { (file, _) =>
    ChangelogNotes.write(List(file, "1.2.0")).left.map(_.headOption) should matchPattern {
      case Left(Some(message: String)) if message.startsWith("usage:") =>
    }
  }
