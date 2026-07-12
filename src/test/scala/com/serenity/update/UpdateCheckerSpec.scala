package com.serenity.update

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UpdateCheckerSpec extends AnyFlatSpec with Matchers:

  "UpdateChecker" should "report an available matching asset from a newer release" in {
    val payload =
      """{"tag_name":"desktop-latest","target_commitish":"new-commit","assets":[{"name":"Serenity.jar","browser_download_url":"https://example.test/Serenity.jar"}]}"""

    UpdateChecker.check("old-commit", UpdatePlatform.Jar)(IO.pure(payload)).unsafeRunSync() shouldBe
      Right(
        UpdateCheckResult.Available("desktop-latest", ReleaseAsset("Serenity.jar", "https://example.test/Serenity.jar"))
      )
  }

  it should "report a current build without selecting an asset" in {
    val payload = """{"tag_name":"desktop-latest","target_commitish":"current-commit","assets":[]}"""

    UpdateChecker.check("current-commit", UpdatePlatform.Jar)(IO.pure(payload)).unsafeRunSync() shouldBe
      Right(UpdateCheckResult.Current)
  }
end UpdateCheckerSpec
