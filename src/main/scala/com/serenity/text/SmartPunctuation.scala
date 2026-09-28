package com.serenity.text

/** As-you-type punctuation autoformatting: straight quotes become curly, two hyphens become an em dash, and three
  * periods become a true ellipsis -- the same conveniences prose-writing tools like Neo offer, gated behind
  * `languageToolsConfig.smartPunctuationEnabled` so code buffers (where a literal `--` or `"` matters) are unaffected
  * by default.
  */
object SmartPunctuation:

  private val EmDash           = "—"
  private val Ellipsis         = "…"
  private val LeftDoubleQuote  = "“"
  private val RightDoubleQuote = "”"
  private val LeftSingleQuote  = "‘"
  private val RightSingleQuote = "’"

  /** How many characters immediately before the cursor a caller needs to pass as `precedingText` for this to see every
    * rule below -- fewer is fine near the start of a buffer.
    */
  val lookbehind: Int = 2

  /** The replacement for `typed`, given the (up to `lookbehind`-character) text immediately before the cursor. `None`
    * means "insert `typed` unchanged". Otherwise, the first element of the pair is how many of the trailing characters
    * of `precedingText` to replace (counting backward from the cursor), and the second is the full replacement text --
    * `typed` itself is never inserted raw once a rule fires, it's folded into the replacement.
    */
  def replacementFor(typed: Char, precedingText: String): Option[(Int, String)] =
    typed match
      case '-' if precedingText.endsWith("-")  => Some((1, EmDash))
      case '.' if precedingText.endsWith("..") => Some((2, Ellipsis))
      case '"'  => Some((0, if opensQuote(precedingText) then LeftDoubleQuote else RightDoubleQuote))
      case '\'' => Some((0, if opensQuote(precedingText) then LeftSingleQuote else RightSingleQuote))
      case _    => None

  /** A quote opens rather than closes at the start of the buffer, after whitespace, or after an opening bracket/dash --
    * everywhere else (typically right after a letter) it closes, which also covers the common apostrophe case ("don't",
    * "'tis" aside).
    */
  private def opensQuote(precedingText: String): Boolean =
    precedingText.lastOption.forall(c => c.isWhitespace || isOpeningBracket(c))

  private def isOpeningBracket(c: Char): Boolean =
    c == '(' || c == '[' || c == '{' || c == '—' || c == '–'
