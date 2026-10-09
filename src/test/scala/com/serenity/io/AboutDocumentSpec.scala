package com.serenity.io

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{FileSystems, Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.BuildInfo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AboutDocumentSpec extends AnyFlatSpec with Matchers:

  private val aboutInfo = AboutDocument.Info(
    version = "1.2.0",
    commit = "abcd1234ef567890",
    commitTime = "2026-10-01T12:00:00+00:00",
    channel = "release",
    java = "21.0.4+7 Eclipse Adoptium",
    os = "Linux 6.1.0 (amd64)",
    configFile = Path.of("/home/u/.serenity/config.conf"),
    sessionDirectory = Path.of("/home/u/.serenity"),
    logFile = Path.of("/home/u/.serenity/serenity.log"),
    lspLogDirectory = Path.of("/home/u/.serenity/logs")
  )

  private def render(privacy: Option[String] = Some("# Privacy\n\nNo telemetry.\n")): String =
    AboutDocument.render(aboutInfo, "THE LICENCE", "# Third-party notices\n", privacy)

  "AboutDocument.render" should "start with the version, commit, commit time and channel" in {
    val document = render()

    document should startWith("# About Serenity")
    document should include("Version: 1.2.0")
    document should include("Commit: abcd1234ef567890 (2026-10-01T12:00:00+00:00)")
    document should include("Channel: release")
  }

  it should "name the Java runtime and operating system" in {
    render() should include("Java: 21.0.4+7 Eclipse Adoptium")
    render() should include("OS: Linux 6.1.0 (amd64)")
  }

  it should "list where the config, sessions and logs live" in {
    val document = render()

    document should include(s"Config: ${aboutInfo.configFile}")
    document should include(s"Sessions: ${aboutInfo.sessionDirectory}")
    document should include(s"Log: ${aboutInfo.logFile}")
    document should include(s"Language server logs: ${aboutInfo.lspLogDirectory}")
  }

  it should "link to the releases page" in {
    render() should include(s"Releases: ${ReleasesPage.url}")
  }

  it should "include the privacy statement when the build bundles one" in {
    render() should include("# Privacy\n\nNo telemetry.")
  }

  it should "say the statement is missing instead of leaving privacy out when the build does not bundle one" in {
    render(None) should include("not bundled with this build")
  }

  it should "put the about block before the licence and the licence before the notices" in {
    val document = render()

    document.indexOf("# About Serenity") should be < document.indexOf("THE LICENCE")
    document.indexOf("THE LICENCE") should be < document.indexOf("# Third-party notices")
  }

  "AboutDocument.currentInfo" should "describe this build" in {
    val current = AboutDocument.currentInfo
    current.version shouldBe BuildInfo.version
    current.commit shouldBe BuildInfo.commit
    current.commitTime shouldBe BuildInfo.commitTime
    current.channel shouldBe BuildInfo.channel
  }

  "ReleasesPage" should "point at the project's GitHub releases" in {
    ReleasesPage.url shouldBe "https://github.com/BarMal/Serenity/releases"
    ReleasesPage.uri.toString shouldBe ReleasesPage.url
  }

  "AboutDocument.writeReadOnly" should "write the about block, licence and notices, read-only and rewritable" in {
    val directory = Files.createTempDirectory("about-document-spec")
    val written   = AboutDocument.writeReadOnly(directory).unsafeRunSync()
    val rewritten = AboutDocument.writeReadOnly(directory).unsafeRunSync()

    written shouldBe rewritten
    written.getFileName.toString shouldBe AboutDocument.documentName
    assume(FileSystems.getDefault.supportedFileAttributeViews.contains("posix"))
    Files.getPosixFilePermissions(written).asScala should not contain PosixFilePermission.OWNER_WRITE
    val content = Files.readString(written)
    content should startWith("# About Serenity")
    content should include("GNU GENERAL PUBLIC LICENSE")
    content should include("# Third-party notices")
  }
