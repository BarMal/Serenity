package com.serenity.app

import java.nio.file.{Files, Path}
import java.util.Comparator

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.{IO, Resource}
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.state.models.StatusLineText

/** A start that trusts nothing the last one left behind: default settings, no saved session, no language servers, spell
  * check or project tasks. It reads and writes nothing under the user's settings folder, so leaving it returns to
  * exactly what was there before.
  */
object SafeMode:

  val Notice: String =
    "Safe mode: settings and session are not loaded, and language servers, spell check and project tasks are off. " +
      "Nothing on disk is changed."

  def windowTitle(base: String): String =
    s"$base -- ${StatusLineText.SafeModeLabel}"

  val config: AppConfig =
    val serversOff = LanguageId.values.toList.map(language => language.id -> LspServerOverride(None, None, Some(false)))
    val defaults   = AppConfig.default.languageToolsConfig
    AppConfig.default.withLanguageToolsConfig(
      defaults.copy(
        lspUserConfig = LspUserConfig(Some(serversOff.toMap)),
        spellCheck = SpellCheckConfig(enabled = false)
      )
    )

  /** Safe mode writes its session into a scratch folder, so the real one is neither read nor replaced. */
  def sessionRootFor(plan: StartupRecovery.Plan): Resource[IO, Option[Path]] =
    if plan.safeMode then scratchSessionRoot.map(Some(_)) else Resource.pure(None)

  /** Where safe mode keeps the session it writes while running, so the real one is neither read nor replaced. */
  def scratchSessionRoot: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("serenity-safe-mode")))(root =>
      IO.blocking(Using.resource(Files.walk(root))(_.sorted(Comparator.reverseOrder[Path]()).iterator().asScala.toList))
        .flatMap(paths =>
          IO.blocking(paths.foreach { path =>
            val _ = Files.deleteIfExists(path)
          })
        )
        .handleError(_ => ())
    )
