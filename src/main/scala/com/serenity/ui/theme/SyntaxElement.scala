package com.serenity.ui.theme

enum SyntaxElement:
  case Keyword       // if, def, class, etc.
  case String        // String literals
  case Comment       // Comments
  case Number        // Numeric literals
  case Operator      // +, -, *, etc.
  case Identifier    // Variable names
  case Type          // Type names
  case Delimiter     // {, }, (, ), etc.
  case Whitespace    // Spaces, tabs, newlines
  case Error         // Syntax errors
  case Normal        // Default text
  case Heading       // Markdown and rich-text heading text; a theme that does not set it uses Keyword
  case HeadingMarker // The `#` run before a Markdown heading; a theme that does not set it uses Delimiter

object SyntaxElement:

  /** The element a theme's colour for `element` is taken from when the theme does not set one of its own. */
  def fallback(element: SyntaxElement): Option[SyntaxElement] =
    element match
      case Heading       => Some(Keyword)
      case HeadingMarker => Some(Delimiter)
      case _             => None

  /** Maps an LSP semantic token type (`textDocument/semanticTokens`'s legend, LSP 3.17 §3.17.7.4 -- `namespace`,
    * `class`, `keyword`, etc.) onto this theme's coarser vocabulary. This is now the only source of syntax
    * classification (issue #859/#1177): the handwritten regex-based tokenizer this replaced classified line-local text
    * fragments rather than real language tokens, and coerced every language through Scala-shaped keyword/string/number
    * rules.
    */
  def fromLspTokenType(tokenType: String): SyntaxElement =
    tokenType match
      case "keyword" | "modifier"                                                             => Keyword
      case "string" | "regexp"                                                                => String
      case "comment"                                                                          => Comment
      case "number"                                                                           => Number
      case "operator"                                                                         => Operator
      case "namespace" | "class" | "enum" | "interface" | "struct" | "typeParameter" | "type" => Type
      case "parameter" | "variable" | "property" | "enumMember" | "function" | "method" | "macro" | "event" |
          "decorator" | "label" =>
        Identifier
      case _ => Normal
