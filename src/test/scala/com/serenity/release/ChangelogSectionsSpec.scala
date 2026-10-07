package com.serenity.release

import java.nio.file.{Files, Path}

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChangelogSectionsSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val sample =
    """# Changelog
      |
      |## [Unreleased]
      |
      |- Added a thing (#1).
      |
      |## 1.2.0 — 2026-10-06
      |
      |- Added the release thing (#2).
      |- Fixed another (#3).
      |
      |### Notes
      |
      |Subheadings stay inside the section.
      |
      |## 1.1.0-rc.1 — 2026-09-01
      |
      |- First candidate.
      |""".stripMargin

  "ChangelogSections.parse" should "read the unreleased section and each version section in order" in {
    ChangelogSections.parse(sample).value.map(_.heading) shouldBe List(
      ChangelogHeading.Unreleased,
      ChangelogHeading.Release("1.2.0", "2026-10-06"),
      ChangelogHeading.Release("1.1.0-rc.1", "2026-09-01")
    )
  }

  "ChangelogSections.unreleased" should "return the body without the heading or surrounding blank lines" in {
    ChangelogSections.unreleased(sample).value shouldBe "- Added a thing (#1)."
  }

  "ChangelogSections.forVersion" should "return the body of the matching version, keeping sub-headings" in {
    ChangelogSections.forVersion(sample, "1.2.0").value shouldBe
      """- Added the release thing (#2).
        |- Fixed another (#3).
        |
        |### Notes
        |
        |Subheadings stay inside the section.""".stripMargin
  }

  it should "match a prerelease version exactly rather than by prefix" in {
    ChangelogSections.forVersion(sample, "1.1.0-rc.1").value shouldBe "- First candidate."
    ChangelogSections.forVersion(sample, "1.1.0").left.value.head should include("1.1.0")
  }

  it should "fail when the version has no section" in {
    ChangelogSections.forVersion(sample, "9.9.9").left.value.head should include("9.9.9")
  }

  it should "fail when the section exists but is empty" in {
    val empty = "## [Unreleased]\n\n## 1.0.0 — 2026-10-06\n\n## 0.9.0 — 2026-09-01\n\n- Old.\n"

    ChangelogSections.forVersion(empty, "1.0.0").left.value.head should include("empty")
  }

  "ChangelogSections validation" should "fail when there is no unreleased section" in {
    val text = "# Changelog\n\n## 1.0.0 — 2026-10-06\n\n- Something.\n"

    ChangelogSections.parse(text).left.value.head should include("[Unreleased]")
    ChangelogSections.unreleased(text).left.value.head should include("[Unreleased]")
  }

  it should "fail when the unreleased section is not first" in {
    val text = "## 1.0.0 — 2026-10-06\n\n- Something.\n\n## [Unreleased]\n"

    ChangelogSections.parse(text).left.value.head should include("first")
  }

  it should "reject date-only and other malformed level-two headings" in
    List(
      "## 2026-10-06",
      "## Unreleased",
      "## [unreleased]",
      "## 1.2 — 2026-10-06",
      "## v1.2.0 — 2026-10-06",
      "## 1.2.0 - 2026-10-06",
      "## 1.2.0 — 06/10/2026",
      "## 1.2.0 — 2026-13-45",
      "## 1.2.0",
      "## [1.2.0] — 2026-10-06"
    ).foreach { heading =>
      val text = s"## [Unreleased]\n\n$heading\n\n- Something.\n"

      withClue(heading) {
        ChangelogSections.parse(text).left.value.exists(_.contains(heading)) shouldBe true
      }
    }

  it should "report every malformed heading with its line number" in {
    val text = "## [Unreleased]\n\n## nope\n\n## 1.0.0 — 2026-10-06\n\n## also nope\n"

    val errors = ChangelogSections.parse(text).left.value

    errors.count(_.contains("line 3")) shouldBe 1
    errors.count(_.contains("line 7")) shouldBe 1
  }

  it should "reject a duplicated version" in {
    val text = "## [Unreleased]\n\n## 1.0.0 — 2026-10-06\n\n- A.\n\n## 1.0.0 — 2026-10-07\n\n- B.\n"

    ChangelogSections.parse(text).left.value.exists(_.contains("duplicate")) shouldBe true
  }

  it should "ignore level-two lines inside fenced code blocks" in {
    val text = "## [Unreleased]\n\n```\n## not a heading\n```\n\n- Real.\n"

    ChangelogSections.unreleased(text).value should include("## not a heading")
  }

  "The repository CHANGELOG.md" should "have an [Unreleased] section and only well-formed headings" in {
    val text = Files.readString(Path.of("").toAbsolutePath.resolve("CHANGELOG.md"))

    ChangelogSections.parse(text).swap.toOption.map(_.mkString("\n")) shouldBe None
    ChangelogSections.unreleased(text).swap.toOption.map(_.mkString("\n")) shouldBe None
  }
