package com.serenity.lsp

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Real server subprocesses that answer `initialize` and then stop responding. Releasing the connection must still
  * return, and must leave no process behind, or a hung server hangs the editor's shutdown and every server restart.
  */
class LspConnectionHungServerSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionHungServerSpec"))

  private val releaseBound = 10.seconds

  private val javaExecutable: String =
    val name = if System.getProperty("os.name", "").toLowerCase.startsWith("windows") then "java.exe" else "java"
    Paths.get(System.getProperty("java.home"), "bin", name).toString

  private val serverSource: String =
    Paths.get(getClass.getResource("/lsp/HungLspServer.java").toURI).toString

  private def serverConfig(mode: String, pidFile: Path) = LspServerConfig(
    LanguageId.Scala,
    LspServerBinary.Metals,
    defaultArgs = List(serverSource, mode, pidFile.toString),
    commandOverride = Some(javaExecutable)
  )

  private def handle(pidFile: Path): Option[ProcessHandle] =
    Option(Files.readString(pidFile).trim)
      .filter(_.nonEmpty)
      .flatMap(pid => Option(ProcessHandle.of(pid.toLong).orElse(null)))

  /** Runs one connect-and-release against the hung server, timing only the release: a real subprocess's start-up (a
    * source-launched JVM, slow on Windows runners) is not what the bound is about. The fixture is killed afterwards
    * whatever happened, so a release that hangs fails this spec instead of hanging it.
    */
  private def releaseOutcome(mode: String): (Boolean, Boolean) =
    val pidFile = Files.createTempFile("hung-lsp-server", ".pid")
    try
      val release = for
        connected <- LspConnection(
          serverConfig(mode, pidFile),
          WorkspaceRootUri("file:///workspace"),
          logger,
          20.seconds
        ).allocated
        (_, close) = connected
        fiber    <- close.start
        finished <- fiber.joinWithUnit.as(true).timeoutTo(releaseBound, IO.pure(false))
      yield finished
      val finished = release.unsafeRunSync()
      val alive    = handle(pidFile).exists(_.isAlive)
      (finished, alive)
    finally
      handle(pidFile).foreach(_.destroyForcibly())
      Files.deleteIfExists(pidFile)

  "LspConnection" should "release within a bound and kill a server that ignores shutdown and exit" in {
    val (finished, alive) = releaseOutcome("ignore")
    withClue("release did not complete within the bound: ")(finished shouldBe true)
    withClue("server process still alive after release: ")(alive shouldBe false)
  }

  it should "release within a bound and kill a server that has stopped reading its stdin" in {
    val (finished, alive) = releaseOutcome("deaf")
    withClue("release did not complete within the bound: ")(finished shouldBe true)
    withClue("server process still alive after release: ")(alive shouldBe false)
  }
