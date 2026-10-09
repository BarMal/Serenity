package com.serenity.io

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{FileSystems, Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.command.{CommandIntent, CommandRegistry, FileIntent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The privacy statement ships in the application and as `docs/PRIVACY.md`, and its claim that Serenity makes no
  * network connections of its own is guarded: any new use of `java.net` networking in `src/main` fails here until it is
  * reviewed, the statement updated, and the file added to the allowlist.
  */
class PrivacyStatementSpec extends AnyFlatSpec with Matchers:

  "The Show Privacy Statement command" should "be registered and route to ShowPrivacyStatement" in {
    val command = CommandRegistry.default.findCommand("show-privacy-statement")

    command.map(_.label) shouldBe Some("Show Privacy Statement")
    command.map(_.intent) shouldBe Some(CommandIntent.File(FileIntent.ShowPrivacyStatement))
  }

  "The bundled statement" should "say there is no telemetry and no network connection" in {
    val statement = PrivacyStatement.text.unsafeRunSync().replaceAll("\\s+", " ")

    statement should include("no telemetry")
    statement should include("opens no network connections of its own")
  }

  it should "be the same text as docs/PRIVACY.md" in {
    PrivacyStatement.text.unsafeRunSync() shouldBe Files.readString(Path.of("docs/PRIVACY.md"))
  }

  it should "be written read-only under its document name" in {
    val directory = TestTemp.directory("privacy-statement-spec")
    val written   = PrivacyStatement.writeReadOnly(directory).unsafeRunSync()

    written.getFileName.toString shouldBe PrivacyStatement.documentName
    assume(FileSystems.getDefault.supportedFileAttributeViews.contains("posix"))
    Files.getPosixFilePermissions(written).asScala should not contain PosixFilePermission.OWNER_WRITE
    Files.readString(written) shouldBe PrivacyStatement.text.unsafeRunSync()
  }

  "Application sources" should "use java.net only where the privacy statement has been checked" in {
    val allowlist = Set(
      "com/serenity/markdown/MarkdownPreviewXhtml.scala",
      "com/serenity/markdown/MarkdownDocumentPreview.scala",
      "com/serenity/markdown/MarkdownPreviewImageResources.scala",
      "com/serenity/lsp/LspManager.scala",
      "com/serenity/io/StorageLocation.scala",
      "com/serenity/app/instance/InstanceMessenger.scala",
      "com/serenity/io/ExternalBrowser.scala",
      "com/serenity/io/ReleasesPage.scala",
      "com/serenity/state/manager/ReleasesPageEffect.scala",
      "com/serenity/state/manager/StateManagerCapabilityPorts.scala"
    )
    val networking =
      """java\.net\.|HttpClient|HttpURLConnection|\bURL\(|InetSocketAddress|InetAddress|DatagramSocket|\bnew Socket\(|\bServerSocket\(|WebSocket""".r
    val root = Path.of("src/main/scala")

    val offenders = Using
      .resource(Files.walk(root))(_.iterator.asScala.toList)
      .filter(_.toString.endsWith(".scala"))
      .map(path => root.relativize(path).toString.replace('\\', '/') -> Files.readString(path))
      .collect { case (name, source) if networking.findFirstIn(source).isDefined && !allowlist.contains(name) => name }

    offenders shouldBe empty
  }
