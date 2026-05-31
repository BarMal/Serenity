package com.serenity.lsp.config

enum LspServerBinary(val command: String, val languageId: LanguageId):
  case Metals       extends LspServerBinary("metals",          LanguageId.Scala)
  case RustAnalyzer extends LspServerBinary("rust-analyzer",   LanguageId.Rust)
  case Gopls        extends LspServerBinary("gopls",           LanguageId.Go)
  case Pyright      extends LspServerBinary("pyright-langserver", LanguageId.Python)
  case Pylsp        extends LspServerBinary("pylsp",           LanguageId.Python)
  case TsServer     extends LspServerBinary("typescript-language-server", LanguageId.TypeScript)
  case JsServer     extends LspServerBinary("typescript-language-server", LanguageId.JavaScript)
  case Clangd       extends LspServerBinary("clangd",          LanguageId.Cpp)
  case ClangdC      extends LspServerBinary("clangd",          LanguageId.C)
  case Jdtls        extends LspServerBinary("jdtls",           LanguageId.Java)
  case KotlinLs     extends LspServerBinary("kotlin-language-server", LanguageId.Kotlin)
  case OmniSharp    extends LspServerBinary("OmniSharp",       LanguageId.CSharp)
  case HaskellLs    extends LspServerBinary("haskell-language-server-wrapper", LanguageId.Haskell)
  case Solargraph   extends LspServerBinary("solargraph",      LanguageId.Ruby)
  case LuaLs        extends LspServerBinary("lua-language-server", LanguageId.Lua)
  case TaploLs      extends LspServerBinary("taplo",           LanguageId.Toml)
  case JsonLs       extends LspServerBinary("vscode-json-language-server", LanguageId.JsonLang)
  case YamlLs       extends LspServerBinary("yaml-language-server", LanguageId.Yaml)
  case MarkdownLs   extends LspServerBinary("marksman",        LanguageId.Markdown)
  case HtmlLs       extends LspServerBinary("vscode-html-language-server", LanguageId.Html)
  case CssLs        extends LspServerBinary("vscode-css-language-server", LanguageId.Css)
  case XmlLs        extends LspServerBinary("lemminx",         LanguageId.Xml)
  case SqlLs        extends LspServerBinary("sqls",            LanguageId.Sql)

object LspServerBinary:
  def forLanguage(languageId: LanguageId): List[LspServerBinary] =
    values.filter(_.languageId == languageId).toList

  def fromCommand(command: String): Option[LspServerBinary] =
    values.find(_.command == command)
