package com.serenity.state.models

import java.nio.file.Path

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The size policy for large files (#1867): where a document is merely large, and where it is too large for tooling. */
class LargeFilePolicySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val policy = LargeFilePolicy(warnAboveChars = 10, toolingOffAboveChars = 20)
  private val path   = Path.of("/workspace/big.log")

  private def documentOf(chars: Int): Document = Document(Rope("x" * chars), filePath = Some(path))

  "LargeFilePolicy.classify" should "leave a document at the warning threshold ordinary and one above it large" in {
    policy.classify(10) shouldBe FileSize.Ordinary
    policy.classify(11) shouldBe FileSize.Large
  }

  it should "call a document above the tooling limit huge" in {
    policy.classify(20) shouldBe FileSize.Large
    policy.classify(21) shouldBe FileSize.Huge
  }

  "A large document" should "keep language tooling and spell check" in {
    val document = documentOf(15)

    policy.allowsLanguageTooling(document) shouldBe true
    policy.allowsSpellCheck(document) shouldBe true
  }

  "A huge document" should "lose language tooling and spell check" in {
    val document = documentOf(21)

    policy.allowsLanguageTooling(document) shouldBe false
    policy.allowsSpellCheck(document) shouldBe false
  }

  "The opening notice" should "be absent for an ordinary document" in {
    policy.openingNotice(path, documentOf(5)) shouldBe None
  }

  it should "warn that a large document may edit slowly" in {
    val notice = policy.openingNotice(path, documentOf(15))

    notice.map(_.level) shouldBe Some(NoticeLevel.Warning)
    notice.map(_.message) shouldBe Some("big.log is large (0.0 million characters): editing it may be slow.")
  }

  it should "say which features a huge document goes without" in {
    val notice = policy.openingNotice(path, documentOf(21))

    notice.map(_.message) shouldBe Some(
      "big.log is very large (0.0 million characters): language server features and spell check are off."
    )
  }

  "A policy with optimizations off" should "warn about a huge document but never switch its tooling off" in {
    val unoptimized = policy.copy(optimizations = false)

    unoptimized.classify(21) shouldBe FileSize.Large
    unoptimized.allowsLanguageTooling(documentOf(21)) shouldBe true
    unoptimized.allowsSpellCheck(documentOf(21)) shouldBe true
  }

  "The default policy" should "warn at five million characters and switch tooling off at twenty million" in {
    LargeFilePolicy.Default.classify(5000000) shouldBe FileSize.Ordinary
    LargeFilePolicy.Default.classify(5000001) shouldBe FileSize.Large
    LargeFilePolicy.Default.classify(20000001) shouldBe FileSize.Huge
  }
