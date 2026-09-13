package com.serenity.ui.theme

enum SyntaxElement:
  case Keyword    // if, def, class, etc.
  case String     // String literals
  case Comment    // Comments
  case Number     // Numeric literals
  case Operator   // +, -, *, etc.
  case Identifier // Variable names
  case Type       // Type names
  case Delimiter  // {, }, (, ), etc.
  case Whitespace // Spaces, tabs, newlines
  case Error      // Syntax errors
  case Normal     // Default text

object SyntaxElement:

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
