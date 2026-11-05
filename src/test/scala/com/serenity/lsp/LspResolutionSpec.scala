package com.serenity.lsp

import java.io.File
import java.nio.file.{Path, Paths}

import com.serenity.lsp.config.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Why a language has no language server running for it (#2523): there is none to run, the user turned it off, or it is
  * not installed. Each asks something different of the user, so none may be answered with another's notice. The rules
  * hold for every language in the registry, so they are checked over all of them.
  */
class LspResolutionSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val everything: String => Boolean = _ => true
  private val nothing: String => Boolean    = _ => false

  private def disabled(language: LanguageId): LspUserConfig =
    LspUserConfig(Some(Map(language.id -> LspServerOverride(command = None, args = None, enabled = Some(false)))))

  private def overridden(language: LanguageId, command: String): LspUserConfig =
    LspUserConfig(Some(Map(language.id -> LspServerOverride(Some(command), None, Some(true)))))

  private def resolution(language: LanguageId, user: LspUserConfig, onPath: String => Boolean) =
    LspServerRegistry.resolution(language, user, onPath)

  "A language the registry has no server for" should "have no server to run, whatever the user configured" in {
    val language = LanguageId.Scala

    LspServerRegistry.resolution(language, LspUserConfig.empty, everything, servers = Nil) shouldBe
      LspResolution.NoServerExists
    LspServerRegistry.resolution(language, overridden(language, "mine"), everything, servers = Nil) shouldBe
      LspResolution.NoServerExists
  }

  "A language the user disabled" should "resolve to Disabled, installed or not" in
    LspServerRegistry.builtIn.map(_.languageId).foreach { language =>
      resolution(language, disabled(language), everything) shouldBe LspResolution.Disabled
      resolution(language, disabled(language), nothing) shouldBe LspResolution.Disabled
    }

  "A configured server that is not on the path" should "resolve to NotInstalled, carrying the server to name" in
    LspServerRegistry.builtIn.foreach { server =>
      resolution(server.languageId, LspUserConfig.empty, nothing) shouldBe LspResolution.NotInstalled(server)
    }

  it should "name the command the user configured" in {
    val language = LspServerRegistry.builtIn.map(_.languageId).headOption.value

    resolution(language, overridden(language, "my-server"), nothing) match
      case LspResolution.NotInstalled(server) => server.command shouldBe "my-server"
      case other                              => fail(s"expected NotInstalled, got $other")
  }

  it should "be probed by that command, not the built-in one" in {
    val language = LspServerRegistry.builtIn.map(_.languageId).headOption.value

    resolution(language, overridden(language, "my-server"), _ == "my-server") match
      case LspResolution.Available(server) => server.command shouldBe "my-server"
      case other                           => fail(s"expected Available, got $other")
  }

  "A configured server that is on the path" should "resolve to Available" in
    LspServerRegistry.builtIn.foreach { server =>
      resolution(server.languageId, LspUserConfig.empty, everything) shouldBe LspResolution.Available(server)
    }

  "The available server of a resolution" should "be present only when the server is available" in {
    val server = LspServerRegistry.builtIn.headOption.value

    LspResolution.Available(server).availableServer shouldBe Some(server)
    LspResolution.NotInstalled(server).availableServer shouldBe None
    LspResolution.Disabled.availableServer shouldBe None
    LspResolution.NoServerExists.availableServer shouldBe None
  }

  private def path(dirs: String*): String = dirs.mkString(File.pathSeparator)

  private def executables(found: Path*): Path => Boolean = found.toSet

  "Finding a command on the path" should "see an executable in any listed directory" in {
    val isExecutable = executables(Paths.get("/opt/tools", "server"))

    LspServerRegistry.isOnPath("server", path("/usr/bin", "/opt/tools"), isWindows = false, isExecutable) shouldBe true
  }

  it should "not find a command that no listed directory holds" in {
    val isExecutable = executables(Paths.get("/elsewhere", "server"))

    LspServerRegistry.isOnPath("server", path("/usr/bin", "/opt/tools"), isWindows = false, isExecutable) shouldBe false
  }

  it should "try the Windows executable suffixes only on Windows" in {
    val isExecutable = executables(Paths.get("/opt/tools", "server.cmd"))

    LspServerRegistry.isOnPath("server", path("/opt/tools"), isWindows = true, isExecutable) shouldBe true
    LspServerRegistry.isOnPath("server", path("/opt/tools"), isWindows = false, isExecutable) shouldBe false
  }

  it should "find nothing on an empty path" in {
    LspServerRegistry.isOnPath("server", "", isWindows = false, executables()) shouldBe false
  }
