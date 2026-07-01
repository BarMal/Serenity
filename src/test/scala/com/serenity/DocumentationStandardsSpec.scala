package com.serenity

import java.nio.file.{Files, Path}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentationStandardsSpec extends AnyFlatSpec with Matchers:

  private val repoRoot = Path.of("").toAbsolutePath

  "Coding standards documentation" should "cover project engineering conventions" in {
    val standardsPath = repoRoot.resolve("docs").resolve("coding-standards.md")

    Files.exists(standardsPath) shouldBe true

    val standards = Files.readString(standardsPath)

    List(
      "Functional Purity",
      "Mutation Policy",
      "Module Boundaries",
      "Configuration And Schema Ownership",
      "Testing Standards"
    ).foreach(heading => standards should include(s"## $heading"))
  }

  it should "be linked from the development guide" in {
    val development = Files.readString(repoRoot.resolve("DEVELOPMENT.md"))

    development should include("docs/coding-standards.md")
  }
