package com.serenity.lsp.config

import java.nio.file.{Files, Paths}

import cats.effect.IO

object LspServerRegistry:

  val builtIn: List[LspServerConfig] = List(
    LspServerConfig(LanguageId.Scala, LspServerBinary.Metals),
    LspServerConfig(LanguageId.Rust, LspServerBinary.RustAnalyzer),
    LspServerConfig(LanguageId.Go, LspServerBinary.Gopls),
    LspServerConfig(LanguageId.Python, LspServerBinary.Pyright),
    LspServerConfig(LanguageId.TypeScript, LspServerBinary.TsServer, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.JavaScript, LspServerBinary.JsServer, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.Cpp, LspServerBinary.Clangd),
    LspServerConfig(LanguageId.C, LspServerBinary.ClangdC),
    LspServerConfig(LanguageId.Java, LspServerBinary.Jdtls),
    LspServerConfig(LanguageId.Kotlin, LspServerBinary.KotlinLs),
    LspServerConfig(LanguageId.CSharp, LspServerBinary.OmniSharp),
    LspServerConfig(LanguageId.Haskell, LspServerBinary.HaskellLs),
    LspServerConfig(LanguageId.Ruby, LspServerBinary.Solargraph),
    LspServerConfig(LanguageId.Lua, LspServerBinary.LuaLs),
    LspServerConfig(LanguageId.Toml, LspServerBinary.TaploLs),
    LspServerConfig(LanguageId.JsonLang, LspServerBinary.JsonLs, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.Yaml, LspServerBinary.YamlLs, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.Markdown, LspServerBinary.MarkdownLs),
    LspServerConfig(LanguageId.Html, LspServerBinary.HtmlLs, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.Css, LspServerBinary.CssLs, defaultArgs = List("--stdio")),
    LspServerConfig(LanguageId.Xml, LspServerBinary.XmlLs),
    LspServerConfig(LanguageId.Sql, LspServerBinary.SqlLs)
  )

  def resolve(languageId: LanguageId, userConfig: LspUserConfig): IO[Option[LspServerConfig]] =
    IO.blocking {
      val base = builtIn.find(_.languageId == languageId)
      base.flatMap { config =>
        val overridden = applyUserOverride(config, languageId, userConfig)
        if isAvailable(overridden.binary.command) then Some(overridden) else None
      }
    }

  def availableServers(userConfig: LspUserConfig): IO[List[LspServerConfig]] =
    IO.blocking {
      builtIn.flatMap { config =>
        val overridden = applyUserOverride(config, config.languageId, userConfig)
        if isAvailable(overridden.binary.command) then Some(overridden) else None
      }
    }

  private def applyUserOverride(
    config: LspServerConfig,
    languageId: LanguageId,
    userConfig: LspUserConfig
  ): LspServerConfig =
    val key = languageId.id
    userConfig.servers.flatMap(_.get(key)) match
      case None => config
      case Some(override_) =>
        val binary = override_.command
          .flatMap(LspServerBinary.fromCommand)
          .getOrElse(config.binary)
        val args = override_.args.getOrElse(config.defaultArgs)
        config.copy(binary = binary, defaultArgs = args)

  private def isAvailable(command: String): Boolean =
    val pathDirs = sys.env
      .getOrElse("PATH", "")
      .split(java.io.File.pathSeparator)
      .toList
    val isWindows = sys.props.getOrElse("os.name", "").toLowerCase.contains("windows")
    val suffixes  = if isWindows then List(".exe", ".cmd", ".bat", "") else List("")
    pathDirs.exists(dir => suffixes.exists(suffix => Files.isExecutable(Paths.get(dir, command + suffix))))
