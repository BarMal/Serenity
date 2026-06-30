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
  case WordDocument
  case WordOpenXmlDocument
  case OpenDocumentText
  case RichText
  case Json
  case Xml
  case Yaml
  case Toml
  case Config
  case Sql
  case Shell
  case Unknown

  def displayName: String =
    FileType.displayName(this)

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
      case "md" | "markdown"    => FileType.Markdown
      case "txt"                => FileType.Text
      case "doc"                => FileType.WordDocument
      case "docx"               => FileType.WordOpenXmlDocument
      case "odt"                => FileType.OpenDocumentText
      case "rtf"                => FileType.RichText
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
    case FileType.WordDocument =>
      "Legacy Word Document (.doc, unsupported)"
    case FileType.WordOpenXmlDocument =>
      "Word Open XML Document"
    case FileType.OpenDocumentText =>
      "OpenDocument Text"
    case FileType.RichText =>
      "Rich Text"
    case FileType.Json    => "JSON"
    case FileType.Xml     => "XML"
    case FileType.Yaml    => "YAML"
    case FileType.Toml    => "TOML"
    case FileType.Config  => "Config"
    case FileType.Sql     => "SQL"
    case FileType.Shell   => "Shell"
    case FileType.Unknown => "Unknown"

enum DocumentFormat:
  case PlainText
  case Markdown
  case RichTextDocument
  case SourceCode
  case StructuredText
  case Unknown

case class DocumentFormatCapabilities(
    canOpen: Boolean,
    canSave: Boolean,
    canRender: Boolean,
    canEdit: Boolean,
    preservesRichFormatting: Boolean
)

object DocumentFormat:

  def fromFileType(fileType: FileType): DocumentFormat =
    fileType match
      case FileType.Text =>
        DocumentFormat.PlainText
      case FileType.Markdown =>
        DocumentFormat.Markdown
      case FileType.WordDocument | FileType.WordOpenXmlDocument | FileType.OpenDocumentText | FileType.RichText =>
        DocumentFormat.RichTextDocument
      case FileType.Json | FileType.Xml | FileType.Yaml | FileType.Toml | FileType.Config | FileType.Sql =>
        DocumentFormat.StructuredText
      case FileType.Unknown =>
        DocumentFormat.Unknown
      case _ =>
        DocumentFormat.SourceCode

  def fromPath(path: Path): DocumentFormat =
    fromFileType(FileType.fromPath(path))

  def capabilities(format: DocumentFormat): DocumentFormatCapabilities =
    format match
      case DocumentFormat.PlainText =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.Markdown =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.SourceCode | DocumentFormat.StructuredText =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.Unknown =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.RichTextDocument =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = true
        )

  def capabilities(fileType: FileType): DocumentFormatCapabilities =
    fileType match
      case FileType.WordDocument =>
        DocumentFormatCapabilities(
          canOpen = false,
          canSave = false,
          canRender = false,
          canEdit = false,
          preservesRichFormatting = false
        )
      case FileType.RichText | FileType.OpenDocumentText | FileType.WordOpenXmlDocument =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = true
        )
      case _ =>
        capabilities(fromFileType(fileType))
