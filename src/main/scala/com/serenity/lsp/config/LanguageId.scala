package com.serenity.lsp.config

enum LanguageId(val id: String, val displayName: String):
  case Scala      extends LanguageId("scala", "Scala")
  case Python     extends LanguageId("python", "Python")
  case Rust       extends LanguageId("rust", "Rust")
  case TypeScript extends LanguageId("typescript", "TypeScript")
  case JavaScript extends LanguageId("javascript", "JavaScript")
  case Go         extends LanguageId("go", "Go")
  case Java       extends LanguageId("java", "Java")
  case Kotlin     extends LanguageId("kotlin", "Kotlin")
  case CSharp     extends LanguageId("csharp", "C#")
  case Cpp        extends LanguageId("cpp", "C++")
  case C          extends LanguageId("c", "C")
  case Haskell    extends LanguageId("haskell", "Haskell")
  case Ruby       extends LanguageId("ruby", "Ruby")
  case Lua        extends LanguageId("lua", "Lua")
  case Toml       extends LanguageId("toml", "TOML")
  case JsonLang   extends LanguageId("json", "JSON")
  case Yaml       extends LanguageId("yaml", "YAML")
  case Markdown   extends LanguageId("markdown", "Markdown")
  case Html       extends LanguageId("html", "HTML")
  case Css        extends LanguageId("css", "CSS")
  case Xml        extends LanguageId("xml", "XML")
  case Sql        extends LanguageId("sql", "SQL")

object LanguageId:
  def fromString(id: String): Option[LanguageId] =
    values.find(_.id == id.toLowerCase)
