package com.serenity.lsp.config

enum RootMarker(val filename: String, val languageIds: Set[LanguageId]):
  case BuildSbt       extends RootMarker("build.sbt",      Set(LanguageId.Scala))
  case MetalsDir      extends RootMarker(".metals",        Set(LanguageId.Scala))
  case BspDir         extends RootMarker(".bsp",           Set(LanguageId.Scala, LanguageId.Java, LanguageId.Kotlin))
  case CargoToml      extends RootMarker("Cargo.toml",     Set(LanguageId.Rust))
  case GoMod          extends RootMarker("go.mod",         Set(LanguageId.Go))
  case PackageJson    extends RootMarker("package.json",   Set(LanguageId.TypeScript, LanguageId.JavaScript))
  case TsConfig       extends RootMarker("tsconfig.json",  Set(LanguageId.TypeScript))
  case PyprojectToml  extends RootMarker("pyproject.toml", Set(LanguageId.Python))
  case SetupPy        extends RootMarker("setup.py",       Set(LanguageId.Python))
  case PomXml         extends RootMarker("pom.xml",        Set(LanguageId.Java, LanguageId.Kotlin))
  case GradleBuild    extends RootMarker("build.gradle",   Set(LanguageId.Java, LanguageId.Kotlin))
  case GradleKts      extends RootMarker("build.gradle.kts", Set(LanguageId.Kotlin))
  case CmakeLists     extends RootMarker("CMakeLists.txt", Set(LanguageId.Cpp, LanguageId.C))
  case MakeFile       extends RootMarker("Makefile",       Set(LanguageId.C, LanguageId.Cpp))
  case CabalProject   extends RootMarker("cabal.project",  Set(LanguageId.Haskell))
  case StackYaml      extends RootMarker("stack.yaml",     Set(LanguageId.Haskell))
  case Gemfile        extends RootMarker("Gemfile",        Set(LanguageId.Ruby))

object RootMarker:
  def forLanguage(languageId: LanguageId): List[RootMarker] =
    values.filter(_.languageIds.contains(languageId)).toList

  def fromFilename(name: String): Option[RootMarker] =
    values.find(_.filename == name)
