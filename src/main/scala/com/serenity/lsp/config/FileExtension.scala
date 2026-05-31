package com.serenity.lsp.config

enum FileExtension(val ext: String, val languageId: LanguageId):
  case ScalaSource  extends FileExtension("scala", LanguageId.Scala)
  case Sbt          extends FileExtension("sbt",   LanguageId.Scala)
  case ScalaScript  extends FileExtension("sc",    LanguageId.Scala)
  case PythonSource extends FileExtension("py",    LanguageId.Python)
  case RustSource   extends FileExtension("rs",    LanguageId.Rust)
  case TypeScriptSource extends FileExtension("ts", LanguageId.TypeScript)
  case TypeScriptJsx    extends FileExtension("tsx", LanguageId.TypeScript)
  case JavaScriptSource extends FileExtension("js",  LanguageId.JavaScript)
  case JavaScriptJsx    extends FileExtension("jsx", LanguageId.JavaScript)
  case GoSource     extends FileExtension("go",    LanguageId.Go)
  case JavaSource   extends FileExtension("java",  LanguageId.Java)
  case KotlinSource extends FileExtension("kt",    LanguageId.Kotlin)
  case KotlinScript extends FileExtension("kts",   LanguageId.Kotlin)
  case CSharpSource extends FileExtension("cs",    LanguageId.CSharp)
  case CppSource    extends FileExtension("cpp",   LanguageId.Cpp)
  case CppHeader    extends FileExtension("hpp",   LanguageId.Cpp)
  case CppCc        extends FileExtension("cc",    LanguageId.Cpp)
  case CSource      extends FileExtension("c",     LanguageId.C)
  case CHeader      extends FileExtension("h",     LanguageId.C)
  case HaskellSource extends FileExtension("hs",   LanguageId.Haskell)
  case RubySource   extends FileExtension("rb",    LanguageId.Ruby)
  case LuaSource    extends FileExtension("lua",   LanguageId.Lua)
  case TomlSource   extends FileExtension("toml",  LanguageId.Toml)
  case JsonSource   extends FileExtension("json",  LanguageId.JsonLang)
  case YamlSource   extends FileExtension("yaml",  LanguageId.Yaml)
  case YmlSource    extends FileExtension("yml",   LanguageId.Yaml)
  case MarkdownSource extends FileExtension("md",  LanguageId.Markdown)
  case HtmlSource   extends FileExtension("html",  LanguageId.Html)
  case HtmlShort    extends FileExtension("htm",   LanguageId.Html)
  case CssSource    extends FileExtension("css",   LanguageId.Css)
  case XmlSource    extends FileExtension("xml",   LanguageId.Xml)
  case SqlSource    extends FileExtension("sql",   LanguageId.Sql)

object FileExtension:
  def fromString(ext: String): Option[FileExtension] =
    values.find(_.ext == ext.toLowerCase.stripPrefix("."))

  def languageIdFor(ext: String): Option[LanguageId] =
    fromString(ext).map(_.languageId)

  def forLanguage(languageId: LanguageId): List[FileExtension] =
    values.filter(_.languageId == languageId).toList
