package com.serenity.io

import java.nio.file.Path

enum FileType:
  case Scala
  case Java
  case JavaScript
  case TypeScript
  case Python
  case Rust
  case Go
  case C
  case Cpp
  case Header
  case Markdown
  case Text
  case Json
  case Xml
  case Yaml
  case Toml
  case Config
  case Sql
  case Shell
  case Unknown

object FileType:

  /** Detect file type from path extension */
  def fromPath(path: Path): FileType =
    val fileName = path.getFileName.toString.toLowerCase
    fileName.lastIndexOf('.') match
      case -1 => FileType.Unknown
      case dotIndex =>
        val extension = fileName.substring(dotIndex + 1)
        fromExtension(extension)

  /** Detect file type from extension string */
  def fromExtension(extension: String): FileType =
    extension.toLowerCase match
      case "scala" | "sc"       => FileType.Scala
      case "java"               => FileType.Java
      case "js" | "mjs"         => FileType.JavaScript
      case "ts"                 => FileType.TypeScript
      case "py"                 => FileType.Python
      case "rs"                 => FileType.Rust
      case "go"                 => FileType.Go
      case "c"                  => FileType.C
      case "cpp" | "cc" | "cxx" => FileType.Cpp
      case "h" | "hpp"          => FileType.Header
      case "md"                 => FileType.Markdown
      case "txt"                => FileType.Text
      case "json"               => FileType.Json
      case "xml"                => FileType.Xml
      case "yaml" | "yml"       => FileType.Yaml
      case "toml"               => FileType.Toml
      case "conf" | "config"    => FileType.Config
      case "sql"                => FileType.Sql
      case "sh" | "bash"        => FileType.Shell
      case _                    => FileType.Unknown

  /** Get display name for file type */
  def displayName(fileType: FileType): String = fileType match
    case FileType.Scala      => "Scala"
    case FileType.Java       => "Java"
    case FileType.JavaScript => "JavaScript"
    case FileType.TypeScript => "TypeScript"
    case FileType.Python     => "Python"
    case FileType.Rust       => "Rust"
    case FileType.Go         => "Go"
    case FileType.C          => "C"
    case FileType.Cpp        => "C++"
    case FileType.Header     => "Header"
    case FileType.Markdown   => "Markdown"
    case FileType.Text       => "Text"
    case FileType.Json       => "JSON"
    case FileType.Xml        => "XML"
    case FileType.Yaml       => "YAML"
    case FileType.Toml       => "TOML"
    case FileType.Config     => "Config"
    case FileType.Sql        => "SQL"
    case FileType.Shell      => "Shell"
    case FileType.Unknown    => "Unknown"
