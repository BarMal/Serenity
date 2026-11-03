package com.serenity.lsp.config

import java.nio.file.{Files, Path, Paths}

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
    IO.blocking(resolution(languageId, userConfig).server)

  /** Why `languageId` has a server or not. `servers` and `onPath` are parameters so the rules can be checked for any
    * registry and any set of installed commands.
    */
  def resolution(
    languageId: LanguageId,
    userConfig: LspUserConfig,
    onPath: String => Boolean = isAvailable,
    servers: List[LspServerConfig] = builtIn
  ): LspResolution =
    if !servers.exists(_.languageId == languageId) then LspResolution.NoServerExists
    else
      configuredServer(languageId, userConfig, servers) match
        case None                                   => LspResolution.Disabled
        case Some(server) if onPath(server.command) => LspResolution.Available(server)
        case Some(server)                           => LspResolution.NotInstalled(server)

  def availableServers(userConfig: LspUserConfig): IO[List[LspServerConfig]] =
    IO.blocking {
      builtIn.flatMap(config =>
        configuredServer(config.languageId, userConfig).filter(server => isAvailable(server.command))
      )
    }

  def configuredServer(
    languageId: LanguageId,
    userConfig: LspUserConfig,
    servers: List[LspServerConfig] = builtIn
  ): Option[LspServerConfig] =
    servers.find(_.languageId == languageId).flatMap { config =>
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
    isOnPath(
      command,
      sys.env.getOrElse("PATH", ""),
      sys.props.getOrElse("os.name", "").toLowerCase.contains("windows"),
      Files.isExecutable
    )

  /** Whether some directory of `pathVariable` holds an executable `command`, with the Windows launcher suffixes tried
    * first on Windows.
    */
  def isOnPath(command: String, pathVariable: String, isWindows: Boolean, isExecutable: Path => Boolean): Boolean =
    val suffixes = if isWindows then List(".exe", ".cmd", ".bat", "") else List("")
    pathVariable
      .split(java.io.File.pathSeparator)
      .toList
      .exists(dir => suffixes.exists(suffix => isExecutable(Paths.get(dir, command + suffix))))
