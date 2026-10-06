package com.serenity.release

/** Maps `git describe` output to the numeric `X.Y.Z` that `jpackage --app-version` accepts, and to a release channel.
  *
  * Lives under `project/` because the build calls it, and is also compiled into the test sources so a spec can reach
  * it; the meta-build is Scala 2.12, so this file sticks to syntax both compilers read.
  *
  * Rejecting a 0.x tag, instead of clamping it, is deliberate: macOS packaging refuses a leading 0, and a clamped
  * `1.0.0` would put an installer version on disk that no tag ever named and that could sort above a later real 1.0.0
  * upgrade.
  */
object PackageVersion {

  sealed abstract class Channel(val label: String)
  object Channel {
    case object Release    extends Channel("release")
    case object Prerelease extends Channel("prerelease")
    case object Nightly    extends Channel("nightly")
    case object Dev        extends Channel("dev")
  }

  final case class Result(numeric: String, channel: Channel)

  val FirstVersion: String = "1.0.0"

  private val Describe =
    """^v(\d+)\.(\d+)\.(\d+)(?:-(?!dirty$)([0-9A-Za-z.]+))?(?:-(\d+)-g[0-9a-f]+)?(-dirty)?$""".r
  private val UntaggedSha = """^(?:[0-9a-f]+)?(-dirty)?$""".r

  /** @param describe
    *   output of `git describe --tags --long --match 'v[0-9]*' --always --dirty`; the short form without `--long` is
    *   accepted too, and a bare sha or empty string means no tag is reachable
    * @param nightly
    *   whether this build was asked to be a nightly; an untagged commit is `dev` otherwise
    */
  def fromDescribe(describe: String, nightly: Boolean): Either[String, Result] =
    describe.trim match {
      case Describe(major, minor, patch, prerelease, distance, dirty) =>
        val tagged  = Option(distance).forall(_.toInt == 0)
        val isDirty = dirty != null
        val numeric = s"$major.$minor.$patch"
        if (major.toInt == 0)
          Left(
            s"tag v$numeric has first component 0, which macOS packaging rejects; tag the first release 1.0.0 or later"
          )
        else if (isDirty) Right(Result(numeric, Channel.Dev))
        else if (!tagged) Right(Result(numeric, untaggedChannel(nightly)))
        else if (prerelease != null) Right(Result(numeric, Channel.Prerelease))
        else Right(Result(numeric, Channel.Release))
      case UntaggedSha(dirty) =>
        Right(Result(FirstVersion, if (dirty != null) Channel.Dev else untaggedChannel(nightly)))
      case other =>
        Left(s"cannot read a package version from git describe output '$other'; expected vX.Y.Z[-pre]")
    }

  private def untaggedChannel(nightly: Boolean): Channel =
    if (nightly) Channel.Nightly else Channel.Dev
}
