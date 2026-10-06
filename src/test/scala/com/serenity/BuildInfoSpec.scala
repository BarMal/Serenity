package com.serenity

import com.serenity.app.VersionBanner
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildInfoSpec extends AnyFlatSpec with Matchers:

  "BuildInfo" should "carry a numeric X.Y.Z package version a packager can use" in {
    BuildInfo.packageVersion should fullyMatch regex """[1-9]\d*\.\d+\.\d+"""
  }

  it should "carry one of the known release channels" in {
    Set("release", "prerelease", "nightly", "dev") should contain(BuildInfo.channel)
  }

  it should "carry a non-empty version, commit and commit time" in {
    BuildInfo.version should not be empty
    BuildInfo.commit should not be empty
    BuildInfo.commitTime should not be empty
  }

  "VersionBanner" should "print the version and commit in the shape --version has always used" in {
    VersionBanner.render("1.2.3", "abc123") shouldBe "Serenity 1.2.3 (abc123)"
  }

  it should "describe the running build" in {
    VersionBanner.current shouldBe s"Serenity ${BuildInfo.version} (${BuildInfo.commit})"
  }
