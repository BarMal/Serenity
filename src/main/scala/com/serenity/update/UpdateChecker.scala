package com.serenity.update

import cats.effect.IO

/** Outcome of checking a published release against the running build. */
enum UpdateCheckResult:
  case Current
  case Available(tag: String, asset: ReleaseAsset)

/** Checks release metadata without downloading or replacing an application artifact. */
object UpdateChecker:

  def check(currentCommit: String, platform: UpdatePlatform)(
    fetchRelease: IO[String]
  ): IO[Either[String, UpdateCheckResult]] =
    fetchRelease.attempt.map {
      case Left(error) => Left(Option(error.getMessage).getOrElse(error.getClass.getSimpleName))
      case Right(json) =>
        ReleaseMetadata.parse(json).flatMap { release =>
          if !release.isNewerThan(currentCommit) then Right(UpdateCheckResult.Current)
          else
            release.assetFor(platform) match
              case Some(asset) => Right(UpdateCheckResult.Available(release.tag, asset))
              case None        => Left(s"Release ${release.tag} has no asset for $platform")
        }
    }
