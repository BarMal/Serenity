package com.serenity.release

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PackageVersionSpec extends AnyFlatSpec with Matchers with EitherValues:

  private def resolved(describe: String, nightly: Boolean = false): (String, String) =
    val result = PackageVersion.fromDescribe(describe, nightly).value
    (result.numeric, result.channel.label)

  "PackageVersion.fromDescribe" should "map a release tag to its numeric version on the release channel" in {
    resolved("v1.2.3") shouldBe ("1.2.3" -> "release")
  }

  it should "treat a long-form describe at distance zero as the tag itself" in {
    resolved("v1.2.3-0-gabcd1234") shouldBe ("1.2.3" -> "release")
  }

  it should "drop the prerelease suffix and report the prerelease channel" in {
    resolved("v1.2.3-rc.4") shouldBe ("1.2.3"             -> "prerelease")
    resolved("v1.2.3-rc.4-0-gabcd1234") shouldBe ("1.2.3" -> "prerelease")
  }

  it should "use the last tag's numeric version for an untagged commit, as nightly when requested" in {
    resolved("v1.2.3-7-gabcd1234", nightly = true) shouldBe ("1.2.3"      -> "nightly")
    resolved("v1.2.3-rc.4-7-gabcd1234", nightly = true) shouldBe ("1.2.3" -> "nightly")
  }

  it should "report an untagged commit as dev unless a nightly is requested" in {
    resolved("v1.2.3-7-gabcd1234") shouldBe ("1.2.3" -> "dev")
  }

  it should "fall back to 1.0.0 before any tag exists, including a bare abbreviated sha or an empty describe" in {
    resolved("abcd1234") shouldBe ("1.0.0"                 -> "dev")
    resolved("abcd1234", nightly = true) shouldBe ("1.0.0" -> "nightly")
    resolved("") shouldBe ("1.0.0"                         -> "dev")
    resolved("abcd1234-dirty") shouldBe ("1.0.0"           -> "dev")
  }

  it should "report a dirty tree as dev even when it sits exactly on a release tag" in {
    resolved("v1.2.3-dirty") shouldBe ("1.2.3"                             -> "dev")
    resolved("v1.2.3-0-gabcd1234-dirty") shouldBe ("1.2.3"                 -> "dev")
    resolved("v1.2.3-7-gabcd1234-dirty", nightly = true) shouldBe ("1.2.3" -> "dev")
  }

  it should "reject a 0.x tag, which macOS packaging cannot use, rather than silently clamping it" in {
    PackageVersion.fromDescribe("v0.9.0", nightly = false).left.value should include("0.9.0")
    PackageVersion.fromDescribe("v0.9.0-3-gabcd1234", nightly = true).left.value should include("first component")
  }

  it should "reject a tag that is not a plain numeric version" in {
    PackageVersion.fromDescribe("v1.2", nightly = false).isLeft shouldBe true
    PackageVersion.fromDescribe("desktop-latest", nightly = false).isLeft shouldBe true
  }

  it should "accept multi-digit components" in {
    resolved("v10.20.300") shouldBe ("10.20.300" -> "release")
  }
