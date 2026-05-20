package com.serenity.ui.theme

enum SyntaxElement:
  case Keyword      // if, def, class, etc.
  case String       // String literals
  case Comment      // Comments
  case Number       // Numeric literals
  case Operator     // +, -, *, etc.
  case Identifier   // Variable names
  case Type         // Type names
  case Delimiter    // {, }, (, ), etc.
  case Whitespace   // Spaces, tabs, newlines
  case Error        // Syntax errors
  case Normal       // Default text

object SyntaxElement:
  def fromText(text: String): SyntaxElement =
    text.trim match
      case s if s.isEmpty => Whitespace
      case s if isKeyword(s) => Keyword
      case s if isStringLiteral(s) => String
      case s if isComment(s) => Comment
      case s if isNumber(s) => Number
      case s if isOperator(s) => Operator
      case s if isDelimiter(s) => Delimiter
      case _ => Normal

  private def isKeyword(text: String): Boolean =
    val keywords = Set(
      "def", "val", "var", "if", "else", "while", "for", "do", "try", "catch", "finally",
      "class", "object", "trait", "extends", "with", "import", "package", "private", "protected",
      "override", "abstract", "sealed", "final", "lazy", "implicit", "match", "case", "return",
      "throw", "new", "this", "super", "null", "true", "false", "given", "using", "enum"
    )
    keywords.contains(text)

  private def isStringLiteral(text: String): Boolean =
    (text.startsWith("\"") && text.endsWith("\"")) || 
    (text.startsWith("'") && text.endsWith("'")) ||
    text.startsWith("\"\"\"")

  private def isComment(text: String): Boolean =
    text.startsWith("//") || text.startsWith("/*") || text.startsWith("*")

  private def isNumber(text: String): Boolean =
    text.matches("""^\d+(\.\d+)?[fFdD]?$""") || text.matches("""^0[xX][0-9a-fA-F]+$""")

  private def isOperator(text: String): Boolean =
    val operators = Set("+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", ">>>")
    operators.contains(text) || text.forall("+-*/%=<>!&|^~".contains(_))

  private def isDelimiter(text: String): Boolean =
    Set("(", ")", "{", "}", "[", "]", ",", ";", ":", ".", "?").contains(text)