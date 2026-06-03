package com.serenity.lsp.config

enum LanguageId(val id: String):
  case Scala      extends LanguageId("scala")
  case Python     extends LanguageId("python")
  case Rust       extends LanguageId("rust")
  case TypeScript extends LanguageId("typescript")
  case JavaScript extends LanguageId("javascript")
  case Go         extends LanguageId("go")
  case Java       extends LanguageId("java")
  case Kotlin     extends LanguageId("kotlin")
  case CSharp     extends LanguageId("csharp")
  case Cpp        extends LanguageId("cpp")
  case C          extends LanguageId("c")
  case Haskell    extends LanguageId("haskell")
  case Ruby       extends LanguageId("ruby")
  case Lua        extends LanguageId("lua")
  case Toml       extends LanguageId("toml")
  case JsonLang   extends LanguageId("json")
  case Yaml       extends LanguageId("yaml")
  case Markdown   extends LanguageId("markdown")
  case Html       extends LanguageId("html")
  case Css        extends LanguageId("css")
  case Xml        extends LanguageId("xml")
  case Sql        extends LanguageId("sql")

object LanguageId:
  def fromString(id: String): Option[LanguageId] =
    values.find(_.id == id.toLowerCase)
