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
      configuredServer(languageId, userConfig).filter(config => isAvailable(config.command))
    }

  def availableServers(userConfig: LspUserConfig): IO[List[LspServerConfig]] =
    IO.blocking {
      builtIn.flatMap(config =>
        configuredServer(config.languageId, userConfig).filter(server => isAvailable(server.command))
      )
    }

  def configuredServer(languageId: LanguageId, userConfig: LspUserConfig): Option[LspServerConfig] =
    builtIn.find(_.languageId == languageId).flatMap { config =>
      userConfig.servers.flatMap(_.get(languageId.id)) match
        case Some(override_) if override_.enabled.contains(false) =>
          None
        case Some(override_) =>
          val binary = override_.command
            .flatMap(LspServerBinary.fromCommand)
            .getOrElse(config.binary)
          Some(
            config.copy(
              binary = binary,
              defaultArgs = override_.args.getOrElse(config.defaultArgs),
              commandOverride = override_.command.filter(_ != binary.command)
            )
          )
        case None =>
          Some(config)
    }

  private def isAvailable(command: String): Boolean =
    val pathDirs = sys.env
      .getOrElse("PATH", "")
      .split(java.io.File.pathSeparator)
      .toList
    val isWindows = sys.props.getOrElse("os.name", "").toLowerCase.contains("windows")
    val suffixes  = if isWindows then List(".exe", ".cmd", ".bat", "") else List("")
    pathDirs.exists(dir => suffixes.exists(suffix => Files.isExecutable(Paths.get(dir, command + suffix))))
