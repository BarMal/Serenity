package com.serenity.text

/** As-you-type punctuation autoformatting: straight quotes become curly, `--` an en dash and `---` an em dash (the
  * SmartyPants rule), and three periods a true ellipsis -- the same conveniences prose-writing tools like Neo offer,
  * gated behind `languageToolsConfig.smartPunctuationEnabled`. The caller decides where it applies (prose buffers, and
  * never inside Markdown code); this object only knows the caret's line.
  */
object SmartPunctuation:

  private val EmDash           = "—"
  private val EnDash           = "–"
  private val Ellipsis         = "…"
  private val LeftDoubleQuote  = "“"
  private val RightDoubleQuote = "”"
  private val LeftSingleQuote  = "‘"
  private val RightSingleQuote = "’"

  private val RuleRowCharacters = Set('-', '|', ':', ' ', '\t')

  /** The replacement for `typed`, given the caret's line up to the caret. `None` means "insert `typed` unchanged".
    * Otherwise, the first element of the pair is how many of the trailing characters of `precedingLineText` to replace
    * (counting backward from the cursor), and the second is the full replacement text -- `typed` itself is never
    * inserted raw once a rule fires, it's folded into the replacement.
    */
  def replacementFor(typed: Char, precedingLineText: String): Option[(Int, String)] =
    typed match
      case '-' if isRuleRowSoFar(precedingLineText)  => None
      case '-' if precedingLineText.endsWith(EnDash) => Some((1, EmDash))
      case '-' if precedingLineText.endsWith("-")    => Some((1, EnDash))
      case '.' if precedingLineText.endsWith("..")   => Some((2, Ellipsis))
      case '"'  => Some((0, if opensQuote(precedingLineText) then LeftDoubleQuote else RightDoubleQuote))
      case '\'' => Some((0, if opensQuote(precedingLineText) then LeftSingleQuote else RightSingleQuote))
      case digit if digit.isDigit && opensElision(precedingLineText) => Some((1, RightSingleQuote + digit))
      case _                                                         => None

  /** Whether the caret sits inside a Markdown code span, judged by an odd number of backticks before it on its line. */
  def withinInlineCode(precedingLineText: String): Boolean =
    precedingLineText.count(_ == '`') % 2 == 1

  /** A line holding nothing but hyphens, pipes, colons and spaces so far is a Markdown thematic break, a front-matter
    * fence or a table delimiter row (`---`, `|---|`, `:--:`), whose hyphens must stay literal.
    */
  private def isRuleRowSoFar(precedingLineText: String): Boolean =
    precedingLineText.nonEmpty && precedingLineText.forall(RuleRowCharacters.contains)

  /** A quote opens rather than closes at the start of the line, after whitespace, or after an opening bracket/dash --
    * everywhere else (typically right after a letter) it closes, which also covers the common apostrophe case ("don't",
    * "'tis" aside).
    */
  private def opensQuote(precedingLineText: String): Boolean =
    precedingLineText.lastOption.forall(c => c.isWhitespace || isOpeningBracket(c))

  /** An opening quote straight before a digit was an apostrophe eliding a number (`'90s`), not a quotation (#1954). */
  private def opensElision(precedingLineText: String): Boolean =
    precedingLineText.endsWith(LeftSingleQuote) && opensQuote(precedingLineText.dropRight(1))

  private def isOpeningBracket(c: Char): Boolean =
    c == '(' || c == '[' || c == '{' || c == '—' || c == '–'
