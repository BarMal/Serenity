package com.serenity.lsp

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{LspConnection, LspMethod, LspStderrLog, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A real server subprocess whose stderr outgrows the OS pipe buffer. Servers such as metals, rust-analyzer and pyright
  * log there continuously; once nobody reads it the server blocks mid-write and every request times out.
  */
class LspConnectionStderrSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionStderrSpec"))

  private val requestTimeout = 20.seconds
  private val maxLogBytes    = 64L * 1024

  private val javaExecutable: String =
    val name = if System.getProperty("os.name", "").toLowerCase.startsWith("windows") then "java.exe" else "java"
    Paths.get(System.getProperty("java.home"), "bin", name).toString

  private val serverSource: String =
    Paths.get(getClass.getResource("/lsp/ChattyLspServer.java").toURI).toString

  private val chattyServer = LspServerConfig(
    LanguageId.Scala,
    LspServerBinary.Metals,
    defaultArgs = List(serverSource),
    commandOverride = Some(javaExecutable)
  )

  private def withLogDirectory[A](use: Path => A): A =
    val directory = Files.createTempDirectory("lsp-stderr-spec")
    try use(directory)
    finally Files.walk(directory).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists(_))

  private def connect(directory: Path) =
    LspConnection(
      chattyServer,
      WorkspaceRootUri("file:///workspace"),
      logger,
      requestTimeout,
      stderrLogDirectory = directory,
      stderrLogMaxBytes = maxLogBytes
    )

  private def handshakeMillis(directory: Path): Long =
    val started = System.nanoTime()
    connect(directory).use(_ => IO.unit).unsafeRunSync()
    (System.nanoTime() - started) / 1_000_000L

  private def logSizes(directory: Path): List[Long] =
    Files.list(directory).iterator().asScala.toList.map(Files.size)

  "LspConnection" should "complete the handshake with a server that writes a megabyte to stderr first" in
    withLogDirectory(directory => handshakeMillis(directory) should be < requestTimeout.toMillis)

  it should "keep the server's stderr in a log file the user can read, capped in size" in
    withLogDirectory { directory =>
      handshakeMillis(directory)

      val log = LspStderrLog.pathFor(directory, LanguageId.Scala)
      Files.exists(log) shouldBe true
      new String(Files.readAllBytes(log)) should include("xxxxxxxxxx")
      logSizes(directory).foreach(_ should be <= maxLogBytes)
      logSizes(directory).sum should be <= 2 * maxLogBytes
    }

  it should "still serve notifications after draining the server's stderr" in
    withLogDirectory { directory =>
      val result = connect(directory)
        .use(_.sendNotification(LspMethod("textDocument/didOpen"), Json.obj()).attempt)
        .unsafeRunSync()

      result shouldBe Right(())
    }

  "LspStderrLog" should "name the log after the language" in {
    LspStderrLog.pathFor(Paths.get("logs"), LanguageId.Scala).getFileName.toString shouldBe "lsp-scala.log"
  }
