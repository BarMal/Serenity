package com.serenity.update

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReleaseMetadataSpec extends AnyFlatSpec with Matchers:

  "ReleaseMetadata" should "parse a GitHub release and select platform assets" in {
    val release = ReleaseMetadata
      .parse(
        """{"tag_name":"desktop-latest","assets":[{"name":"Serenity.jar","browser_download_url":"https://example.test/Serenity.jar"},{"name":"Serenity-windows-x64.zip","browser_download_url":"https://example.test/windows.zip"}]}"""
      )
      .toOption
      .getOrElse(fail("expected release metadata"))

    release.assetFor(UpdatePlatform.Jar).map(_.name) shouldBe Some("Serenity.jar")
    release.assetFor(UpdatePlatform.Windows).map(_.name) shouldBe Some("Serenity-windows-x64.zip")
  }

  it should "reject release payloads without a tag" in {
    ReleaseMetadata.parse("""{"assets":[]}""").isLeft shouldBe true
  }

  it should "identify a release built from a different commit" in {
    val release = ReleaseMetadata
      .parse("""{"tag_name":"desktop-latest","target_commitish":"new-commit","assets":[]}""")
      .toOption
      .getOrElse(fail("expected release metadata"))

    release.isNewerThan("old-commit") shouldBe true
    release.isNewerThan("new-commit") shouldBe false
  }
end ReleaseMetadataSpec
