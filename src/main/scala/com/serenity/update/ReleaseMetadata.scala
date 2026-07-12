package com.serenity.update

import io.circe.Decoder
import io.circe.parser.decode

/** Downloadable asset published with an application release. */
case class ReleaseAsset(name: String, downloadUrl: String)

/** Target runtime package for an update download. */
enum UpdatePlatform:
  case Jar
  case Windows
  case Linux
  case MacOs

/** Metadata needed to present a published update without downloading it. */
case class ReleaseMetadata(tag: String, targetCommit: Option[String], assets: List[ReleaseAsset]):

  def isNewerThan(currentCommit: String): Boolean =
    targetCommit.exists(_ != currentCommit)

  def assetFor(platform: UpdatePlatform): Option[ReleaseAsset] =
    val expectedName = platform match
      case UpdatePlatform.Jar     => "Serenity.jar"
      case UpdatePlatform.Windows => "Serenity-windows-x64.zip"
      case UpdatePlatform.Linux   => "Serenity-linux-x64.zip"
      case UpdatePlatform.MacOs   => "Serenity-macos-arm64.zip"
    assets.find(_.name == expectedName)

object ReleaseMetadata:
  private case class GitHubAsset(name: String, browser_download_url: String)
  private case class GitHubRelease(tag_name: String, target_commitish: Option[String], assets: List[GitHubAsset])

  private given Decoder[GitHubAsset] = Decoder.forProduct2("name", "browser_download_url")(GitHubAsset.apply)
  private given Decoder[GitHubRelease] =
    Decoder.forProduct3("tag_name", "target_commitish", "assets")(GitHubRelease.apply)

  /** Decode the subset of a GitHub release response required for update discovery. */
  def parse(json: String): Either[String, ReleaseMetadata] =
    decode[GitHubRelease](json).left.map(_.getMessage).flatMap { release =>
      Option
        .when(release.tag_name.trim.nonEmpty)(
          ReleaseMetadata(
            release.tag_name,
            release.target_commitish.filter(_.nonEmpty),
            release.assets.map(asset => ReleaseAsset(asset.name, asset.browser_download_url))
          )
        )
        .toRight("Release payload has no tag")
    }
