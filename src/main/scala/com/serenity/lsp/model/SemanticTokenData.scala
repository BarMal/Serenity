package com.serenity.lsp.model

import scala.annotation.tailrec

/** A document's semantic tokens at absolute positions, packed into one primitive array of [[SemanticTokenData.Stride]]
  * integers per token -- line, start character, length, index into `legend.tokenTypes`, modifier bitset -- ordered by
  * position, the way VS Code keeps them. A token list costs an object per token plus a boxed set per modifier; this
  * costs five integers, and an edit is applied to it by [[acceptEdit]] without leaving the array.
  *
  * Equality is by the tokens held, not by the legend or the array they happen to be packed in.
  */
final class SemanticTokenData private (val legend: SemanticTokensLegend, packed: IArray[Int]):

  import SemanticTokenData.*

  private lazy val typeNames: Vector[String]     = legend.tokenTypes.toVector
  private lazy val modifierNames: Vector[String] = legend.tokenModifiers.toVector

  def size: Int = packed.length / Stride

  def isEmpty: Boolean = packed.isEmpty

  def tokens: List[SemanticToken] = List.tabulate(size)(tokenAt)

  def byLine: Map[Int, List[SemanticToken]] = tokens.groupBy(_.line)

  /** These tokens after the text changed by `change`, so they keep colouring the same characters until the server
    * answers for the new text. A token before the edit stays, one after it moves with the text, and one the edit lies
    * within on a single line grows or shrinks with it; a token the edit cuts across is dropped.
    */
  def acceptEdit(change: TextChangeDiff.Change): SemanticTokenData =
    val edit      = EditGeometry(change)
    val fateAt    = (index: Int) => edit.fateOf(line(index), start(index), length(index))
    val surviving = Array.range(0, size).filter(index => fateAt(index) != Fate.Dropped)
    new SemanticTokenData(
      legend,
      IArray.tabulate(surviving.length * Stride) { slot =>
        val index = surviving(slot / Stride)
        val fate  = fateAt(index)
        slot % Stride match
          case 0 => if fate == Fate.After then line(index) + edit.lineDelta else line(index)
          case 1 => if fate == Fate.After then edit.startAfter(line(index), start(index)) else start(index)
          case 2 => if fate == Fate.Resized then length(index) + edit.lengthDelta else length(index)
          case _ => packed(index * Stride + slot % Stride)
      }
    )

  /** These tokens with those on lines `first` to `last` (inclusive) replaced by the ones `replacement` holds there --
    * what a range response says about the lines it covered, leaving every other line as it was.
    */
  def replaceLines(first: Int, last: Int, replacement: SemanticTokenData): SemanticTokenData =
    val replacedLine = (tokenLine: Int) => tokenLine >= first && tokenLine <= last
    if replacement.legend != legend then
      from(
        (tokens.filterNot(token => replacedLine(token.line)) ++ replacement.tokens.filter(token =>
          replacedLine(token.line)
        )).sortBy(token => (token.line, token.startCharacter))
      )
    else
      val before   = indicesWhere(_ < first)
      val after    = indicesWhere(_ > last)
      val inserted = replacement.indicesWhere(replacedLine)
      new SemanticTokenData(
        legend,
        IArray.unsafeFromArray(gather(before) ++ replacement.gather(inserted) ++ gather(after))
      )

  override def equals(other: Any): Boolean =
    other match
      case that: SemanticTokenData => (this eq that) || tokens == that.tokens
      case _                       => false

  override def hashCode: Int = size

  override def toString: String = s"SemanticTokenData($tokens)"

  private def line(index: Int): Int   = packed(index * Stride)
  private def start(index: Int): Int  = packed(index * Stride + 1)
  private def length(index: Int): Int = packed(index * Stride + 2)

  private def tokenAt(index: Int): SemanticToken =
    val bits = packed(index * Stride + 4)
    SemanticToken(
      line(index),
      start(index),
      length(index),
      typeNames(packed(index * Stride + 3)),
      modifierNames.zipWithIndex.collect { case (modifier, bit) if (bits & (1 << bit)) != 0 => modifier }.toSet
    )

  private def indicesWhere(onLine: Int => Boolean): Array[Int] =
    Array.range(0, size).filter(index => onLine(line(index)))

  private def gather(indices: Array[Int]): Array[Int] =
    Array.tabulate(indices.length * Stride)(slot => packed(indices(slot / Stride) * Stride + slot % Stride))

object SemanticTokenData:

  val Stride: Int = 5

  val empty: SemanticTokenData = new SemanticTokenData(SemanticTokensLegend(Nil, Nil), IArray.empty[Int])

  /** Decodes the `data` of a semantic tokens result (LSP 3.17 §3.17.7.4): five integers per token -- `deltaLine`,
    * `deltaStartChar`, `length`, `tokenType`, `tokenModifiers` -- each token's position given relative to the previous
    * token's. `deltaLine == 0` means the same line, so `deltaStartChar` is then relative to that token's start; a
    * nonzero `deltaLine` starts a new line, so `deltaStartChar` is that line's absolute column.
    *
    * A token whose `tokenType` index is outside the legend is left out, but the deltas are still threaded through it:
    * later tokens are relative to its position, not to the last one kept.
    */
  def decode(data: IArray[Int], legend: SemanticTokensLegend): SemanticTokenData =
    val typeCount = legend.tokenTypes.size
    val total     = data.length / Stride
    val absolute  = new Array[Int](total * Stride)

    @tailrec def loop(index: Int, line: Int, start: Int, written: Int): Int =
      if index == total then written
      else
        val base       = index * Stride
        val tokenLine  = line + data(base)
        val tokenStart = if data(base) == 0 then start + data(base + 1) else data(base + 1)
        val typeIndex  = data(base + 3)
        if typeIndex >= 0 && typeIndex < typeCount then
          val slot = written * Stride
          absolute(slot) = tokenLine
          absolute(slot + 1) = tokenStart
          absolute(slot + 2) = data(base + 2)
          absolute(slot + 3) = typeIndex
          absolute(slot + 4) = data(base + 4)
          loop(index + 1, tokenLine, tokenStart, written + 1)
        else loop(index + 1, tokenLine, tokenStart, written)

    val written = loop(0, 0, 0, 0)
    new SemanticTokenData(legend, IArray.unsafeFromArray(absolute.take(written * Stride)))

  /** Packs `tokens`, which must already be in position order, against a legend made of the names they use. */
  def from(tokens: List[SemanticToken]): SemanticTokenData =
    val types     = tokens.map(_.tokenType).distinct
    val modifiers = tokens.flatMap(_.tokenModifiers).distinct
    val typeIndex = types.zipWithIndex.toMap
    val modIndex  = modifiers.zipWithIndex.toMap
    val bitsOf = (token: SemanticToken) =>
      token.tokenModifiers.foldLeft(0)((bits, name) => bits | (1 << modIndex.getOrElse(name, 0)))
    val packed = tokens.flatMap(token =>
      List(token.line, token.startCharacter, token.length, typeIndex.getOrElse(token.tokenType, 0), bitsOf(token))
    )
    new SemanticTokenData(SemanticTokensLegend(types, modifiers), IArray.from(packed))

  private enum Fate:
    case Before, After, Resized, Dropped

  /** An edit replacing the characters from (`startLine`, `startChar`) to (`endLine`, `endChar`) with text of
    * `insertedLines` line breaks, whose last line is `insertedLastLineLength` long and whose whole length is
    * `insertedLength`.
    */
  final private case class EditGeometry(
      startLine: Int,
      startChar: Int,
      endLine: Int,
      endChar: Int,
      insertedLines: Int,
      insertedLastLineLength: Int,
      insertedLength: Int
  ):

    def lineDelta: Int   = insertedLines - (endLine - startLine)
    def lengthDelta: Int = insertedLength - (endChar - startChar)

    def fateOf(line: Int, start: Int, length: Int): Fate =
      if line < startLine || (line == startLine && start + length <= startChar) then Fate.Before
      else if line > endLine || (line == endLine && start >= endChar) then Fate.After
      else if line == startLine && line == endLine && insertedLines == 0 && start <= startChar &&
          endChar <= start + length && length + lengthDelta > 0
      then Fate.Resized
      else Fate.Dropped

    /** The column of a token that began after the edit, on the line the edit ended on or a later one. */
    def startAfter(line: Int, start: Int): Int =
      if line != endLine then start
      else if insertedLines == 0 then startChar + insertedLength + (start - endChar)
      else insertedLastLineLength + (start - endChar)

  private object EditGeometry:

    def apply(change: TextChangeDiff.Change): EditGeometry =
      val text = change.text
      EditGeometry(
        change.range.start.line,
        change.range.start.character,
        change.range.end.line,
        change.range.end.character,
        text.count(_ == '\n'),
        text.length - (text.lastIndexOf('\n') + 1),
        text.length
      )
