package com.serenity.rope

import scala.annotation.tailrec

trait Rope(using balance: Balance):
  def weight: Int
  def height: Int
  def newlineCount: Int
  def lastLineLength: Int
  def endsWithNewline: Boolean

  def isWeightBalanced: Boolean
  def isHeightBalanced: Boolean

  /** Boehm, Atkinson and Plass: a rope of depth n is balanced when its length is at least the (n+2)th Fibonacci number.
    *
    * This is the invariant that protects traversal, because `index` and `splitAt` descend by depth. A weight comparison
    * between two children says nothing about depth and cannot be held without rebuilding on every edit.
    */
  def isDepthBalanced: Boolean =
    val index = height + 1 // a leaf is height 1 here where the paper counts it depth 0
    weight == 0 || (index < Rope.minimumWeightsByHeight.size && weight >= Rope.minimumWeightsByHeight(index))

  def concat(that: Rope): Rope = Node(this, that).rebalance

  /** Reorganises the leaves the rope already holds rather than flattening it to a string and re-splitting.
    *
    * Every unbalanced `concat` and `splitAt` falls back here, so this is the cost of an edit. Going via `collect()`
    * copied every character of the document each time; leaves that are already the right size are now reused as they
    * are, and only fragments are touched.
    */
  def rebuild: Rope = Rope.fromLeafValues(leafValues)

  /** The non-empty leaf strings, left to right. */
  private[rope] def leafValues: Vector[String] =
    @tailrec
    def go(stack: List[Rope], acc: List[String]): List[String] = stack match
      case Nil => acc
      case head :: rest =>
        head match
          case Node(left, right)            => go(left :: right :: rest, acc)
          case Leaf(value) if value.isEmpty => go(rest, acc)
          case Leaf(value)                  => go(rest, value :: acc)
          case other if other.weight == 0   => go(rest, acc)
          case other                        => go(rest, other.collect() :: acc)

    go(List(this), Nil).reverse.toVector

  def rebalance: Rope

  def index(i: Int): Option[Char]

  def splitAt(index: Int): Option[(Rope, Rope)]

  def insert(index: Int, str: String): Rope =
    splitAt(index) match
      case Some(pre, post) =>
        pre.concat(Leaf(str)).concat(post).rebalance
      case None => this

  def delete(startIndex: Int, endIndex: Int): Rope =
    deleteRight(startIndex, endIndex - startIndex)

  def deleteLeft(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteRight(start, Math.abs(count))
    else if start <= 0 then this
    else
      val actualCount = Math.min(count, start)
      deleteRight(start - actualCount, actualCount)

  def deleteRight(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteLeft(start, Math.abs(count))
    else if start + count > weight then deleteRight(start, weight - start)
    else
      (for
        startAndRest <- splitAt(Math.max(0, start))
        (start, rest) = startAndRest
        restAndEnd <- rest.splitAt(count)
        (_, end) = restAndEnd
      yield Node(start, end).rebalance).getOrElse(this)

  def dropLeft(n: Int): Rope = deleteRight(0, n)

  def dropRight(n: Int): Rope = deleteLeft(weight, n)

  def replace(index: Int, char: Char): Rope =
    if index < 0 || index >= weight then this
    else
      splitAt(index) match
        case Some((l, r)) =>
          l.concat(Leaf(char.toString).concat(r.dropLeft(1)))
        case None => this

  def replaceAll(term: String, replacement: String): Rope =
    searchAll(term).sorted.reverse.foldLeft(this)((rope, index) =>
      rope.delete(index, index + term.length).insert(index, replacement)
    )

  def collect(): String =
    val sb = new StringBuilder
    @tailrec
    def go(stack: List[Rope]): Unit = stack match
      case Nil => ()
      case head :: rest =>
        head match
          case Node(l, r) => go(l :: r :: rest)
          case Leaf(v)    => sb.append(v); go(rest)
    go(List(this))
    sb.toString

  /** Visit the leaf content intersecting a half-open rope range from left to right. */
  final private[rope] def chunksInRange(startIndex: Int, endIndex: Int): Iterator[(Int, String)] =
    val start = math.max(0, math.min(startIndex, weight))
    val end   = math.max(start, math.min(endIndex, weight))

    if start == end then Iterator.empty
    else
      Iterator.unfold(List(this -> 0)) { stack =>
        @tailrec
        def nextChunk(remaining: List[(Rope, Int)]): Option[((Int, String), List[(Rope, Int)])] =
          remaining match
            case Nil => None
            case (rope, baseOffset) :: tail if baseOffset >= end || baseOffset + rope.weight <= start =>
              nextChunk(tail)
            case (node: Node, baseOffset) :: tail =>
              val rightOffset = baseOffset + node.left.weight
              nextChunk((node.left -> baseOffset) :: (node.right -> rightOffset) :: tail)
            case (Leaf(value), baseOffset) :: tail =>
              val chunkStart = math.max(0, start - baseOffset)
              val chunkEnd   = math.min(value.length, end - baseOffset)
              Some((baseOffset + chunkStart, value.substring(chunkStart, chunkEnd)) -> tail)
            case (other, baseOffset) :: tail =>
              val chunkStart = math.max(0, start - baseOffset)
              val chunkEnd   = math.min(other.weight, end - baseOffset)
              val value      = new StringBuilder(chunkEnd - chunkStart)

              @tailrec
              def appendIndexed(index: Int): Unit =
                if index < chunkEnd then
                  other.index(index).foreach(value.append)
                  appendIndexed(index + 1)

              appendIndexed(chunkStart)
              Some((baseOffset + chunkStart, value.toString) -> tail)

        nextChunk(stack)
      }

  override def toString: String = collect()

  def slice(startIndex: Int, endIndex: Int): Rope =
    dropRight(weight - endIndex).dropLeft(startIndex)

  def sliceString(startIndex: Int, endIndex: Int): String =
    val start = math.max(0, math.min(startIndex, weight))
    val end   = math.max(start, math.min(endIndex, weight))
    if start == end then ""
    else
      val value = new StringBuilder(end - start)
      chunksInRange(start, end).foreach { case (_, chunk) => value.append(chunk) }
      value.toString

  def searchAll(term: String): List[Int] =
    if term.isEmpty || term.length > weight then List.empty
    else
      val prefix = searchPrefixTable(term)
      chunksInRange(0, weight)
        .foldLeft((List.empty[Int], 0)) {
          case ((found, matched), (chunkOffset, chunk)) =>
            searchChunk(chunk, chunkOffset, term, prefix, matched, found)
        }
        ._1
        .reverse

  def lineCount: Int =
    newlineCount + 1

  def getLine(lineIndex: Int): Option[String] =
    if lineIndex < 0 || lineIndex > newlineCount then None
    else
      val value = new StringBuilder
      appendLine(this, lineIndex, value)
      Some(value.toString)

  def linesFrom(lineIndex: Int, maxLines: Int): Vector[String] =
    linesIteratorFrom(lineIndex).take(maxLines).map(_._2).toVector

  def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
    if lineIndex < 0 || lineIndex > newlineCount then Iterator.empty
    else
      val initial = LineTraversal(chunksInRange(lineColumnToOffset(lineIndex, 0), weight), "", 0, lineIndex, Nil, false)
      Iterator.unfold(initial)(nextLine)

  /** Resolve a logical line and internal UTF-16 column to the corresponding rope offset. Columns are not grapheme
    * counts; they share the same UTF-16 code-unit contract as Java `String` indexes and `CursorPosition.column`.
    */
  def lineColumnToOffset(line: Int, column: Int): Int =
    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    if targetLine > newlineCount then weight
    else lineColumnToOffsetIn(this, targetLine, targetCol, 0)

  /** Resolve an internal rope offset to a logical line and UTF-16 column pair. The returned column may point inside a
    * multi-code-unit grapheme; user-facing edits should snap through grapheme helpers before mutating text.
    */
  def offsetToLineColumn(offset: Int): (Int, Int) =
    val clamped = math.max(0, math.min(offset, weight))
    offsetToLineColumnIn(this, clamped, 0, 0)

  def search(term: String): Option[Int] =
    if term.isEmpty || term.length > weight then None
    else
      @tailrec
      def loop(offset: Int): Option[Int] =
        if offset > weight - term.length then None
        else if matchesAt(offset, term) then Some(offset)
        else loop(offset + 1)

      loop(0)

  private def matchesAt(offset: Int, term: String): Boolean =
    @tailrec
    def loop(index: Int): Boolean =
      if index >= term.length then true
      else
        this.index(offset + index) match
          case Some(char) if char == term.charAt(index) => loop(index + 1)
          case _                                        => false

    loop(0)

  private def appendLine(rope: Rope, lineIndex: Int, value: StringBuilder): Unit =
    rope match
      case Leaf(leafValue) =>
        appendLeafLine(leafValue, lineIndex, value)
      case node: Node =>
        if lineIndex < node.left.newlineCount then appendLine(node.left, lineIndex, value)
        else if lineIndex > node.left.newlineCount then
          appendLine(node.right, lineIndex - node.left.newlineCount, value)
        else
          if node.left.weight > 0 && !node.left.endsWithNewline then appendLine(node.left, lineIndex, value)
          appendLine(node.right, 0, value)
      case other =>
        appendIndexedLine(other, lineIndex, value)

  private def appendLeafLine(leafValue: String, lineIndex: Int, value: StringBuilder): Unit =
    @tailrec
    def loop(offset: Int, currentLine: Int): Unit =
      if offset < leafValue.length then
        val char = leafValue.charAt(offset)
        if currentLine == lineIndex then
          if char != '\n' then
            value.append(char)
            loop(offset + 1, currentLine)
        else if char == '\n' then loop(offset + 1, currentLine + 1)
        else loop(offset + 1, currentLine)

    loop(0, 0)

  private def appendIndexedLine(rope: Rope, lineIndex: Int, value: StringBuilder): Unit =
    @tailrec
    def loop(offset: Int, currentLine: Int): Unit =
      if offset < rope.weight then
        rope.index(offset) match
          case Some('\n') if currentLine == lineIndex => ()
          case Some('\n')                             => loop(offset + 1, currentLine + 1)
          case Some(char) if currentLine == lineIndex =>
            value.append(char)
            loop(offset + 1, currentLine)
          case Some(_) => loop(offset + 1, currentLine)
          case None    => ()

    loop(0, 0)

  final private case class LineTraversal(
      chunks: Iterator[(Int, String)],
      chunk: String,
      chunkIndex: Int,
      line: Int,
      fragments: List[String],
      finished: Boolean
  )

  @tailrec
  private def nextLine(state: LineTraversal): Option[((Int, String), LineTraversal)] =
    if state.finished then None
    else if state.chunkIndex < state.chunk.length then
      val newlineIndex = state.chunk.indexOf('\n', state.chunkIndex)
      if newlineIndex >= 0 then
        val value = (state.chunk.substring(state.chunkIndex, newlineIndex) :: state.fragments).reverse.mkString
        Some(
          (state.line -> value) ->
            state.copy(chunkIndex = newlineIndex + 1, line = state.line + 1, fragments = Nil)
        )
      else
        nextLine(
          state.copy(
            chunkIndex = state.chunk.length,
            fragments = state.chunk.substring(state.chunkIndex) :: state.fragments
          )
        )
    else if state.chunks.hasNext then nextLine(state.copy(chunk = state.chunks.next()._2, chunkIndex = 0))
    else Some((state.line -> state.fragments.reverse.mkString) -> state.copy(finished = true))

  private def searchChunk(
    chunk: String,
    chunkOffset: Int,
    term: String,
    prefix: Vector[Int],
    matched: Int,
    found: List[Int]
  ): (List[Int], Int) =
    @tailrec
    def scan(index: Int, currentMatch: Int, currentFound: List[Int]): (List[Int], Int) =
      if index >= chunk.length then (currentFound, currentMatch)
      else
        val char          = chunk.charAt(index)
        val matchedPrefix = fallbackMatch(char, term, prefix, currentMatch)
        val nextMatch     = if char == term.charAt(matchedPrefix) then matchedPrefix + 1 else 0
        if nextMatch == term.length then scan(index + 1, 0, chunkOffset + index - term.length + 1 :: currentFound)
        else scan(index + 1, nextMatch, currentFound)

    scan(0, matched, found)

  @tailrec
  private def fallbackMatch(char: Char, term: String, prefix: Vector[Int], matched: Int): Int =
    if matched == 0 || char == term.charAt(matched) then matched
    else fallbackMatch(char, term, prefix, prefix(matched - 1))

  private def searchPrefixTable(term: String): Vector[Int] =
    @tailrec
    def build(index: Int, matched: Int, prefix: Vector[Int]): Vector[Int] =
      if index >= term.length then prefix
      else if term.charAt(index) == term.charAt(matched) then build(index + 1, matched + 1, prefix :+ (matched + 1))
      else if matched > 0 then build(index, prefix(matched - 1), prefix)
      else build(index + 1, 0, prefix :+ 0)

    build(index = 1, matched = 0, prefix = Vector(0))

  private def lineColumnToOffsetIn(rope: Rope, line: Int, column: Int, baseOffset: Int): Int =
    rope match
      case Leaf(value) =>
        leafLineColumnToOffset(value, line, column, baseOffset)
      case node: Node =>
        if line < node.left.newlineCount then lineColumnToOffsetIn(node.left, line, column, baseOffset)
        else if line > node.left.newlineCount then
          lineColumnToOffsetIn(node.right, line - node.left.newlineCount, column, baseOffset + node.left.weight)
        else if node.left.endsWithNewline || node.left.weight == 0 then
          lineColumnToOffsetIn(node.right, 0, column, baseOffset + node.left.weight)
        else if column <= node.left.lastLineLength then lineColumnToOffsetIn(node.left, line, column, baseOffset)
        else lineColumnToOffsetIn(node.right, 0, column - node.left.lastLineLength, baseOffset + node.left.weight)
      case other =>
        indexedLineColumnToOffset(other, line, column, baseOffset)

  private def leafLineColumnToOffset(value: String, line: Int, column: Int, baseOffset: Int): Int =
    @tailrec
    def loop(offset: Int, currentLine: Int, currentColumn: Int): Int =
      if offset >= value.length then baseOffset + value.length
      else
        val char = value.charAt(offset)
        if currentLine == line && currentColumn >= column then baseOffset + offset
        else if currentLine == line && char == '\n' then baseOffset + offset
        else if char == '\n' then loop(offset + 1, currentLine + 1, 0)
        else if currentLine == line then loop(offset + 1, currentLine, currentColumn + 1)
        else loop(offset + 1, currentLine, currentColumn)

    loop(0, 0, 0)

  private def indexedLineColumnToOffset(rope: Rope, line: Int, column: Int, baseOffset: Int): Int =
    @tailrec
    def loop(offset: Int, currentLine: Int, currentColumn: Int): Int =
      if offset >= rope.weight then baseOffset + rope.weight
      else
        rope.index(offset) match
          case None => baseOffset + rope.weight
          case Some(char) =>
            if currentLine == line && currentColumn >= column then baseOffset + offset
            else if currentLine == line && char == '\n' then baseOffset + offset
            else if char == '\n' then loop(offset + 1, currentLine + 1, 0)
            else if currentLine == line then loop(offset + 1, currentLine, currentColumn + 1)
            else loop(offset + 1, currentLine, currentColumn)

    loop(0, 0, 0)

  private def offsetToLineColumnIn(
    rope: Rope,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
    if offset >= rope.weight then advancePosition(rope, baseLine, baseColumn)
    else
      rope match
        case Leaf(value) =>
          leafOffsetToLineColumn(value, offset, baseLine, baseColumn)
        case node: Node =>
          if offset <= node.left.weight then offsetToLineColumnIn(node.left, offset, baseLine, baseColumn)
          else
            val (rightBaseLine, rightBaseColumn) = advancePosition(node.left, baseLine, baseColumn)
            offsetToLineColumnIn(node.right, offset - node.left.weight, rightBaseLine, rightBaseColumn)
        case other =>
          indexedOffsetToLineColumn(other, offset, baseLine, baseColumn)

  private def advancePosition(rope: Rope, baseLine: Int, baseColumn: Int): (Int, Int) =
    if rope.weight == 0 then (baseLine, baseColumn)
    else if rope.newlineCount == 0 then (baseLine, baseColumn + rope.weight)
    else (baseLine + rope.newlineCount, rope.lastLineLength)

  private def leafOffsetToLineColumn(
    value: String,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
    @tailrec
    def loop(index: Int, line: Int, column: Int): (Int, Int) =
      if index >= offset || index >= value.length then (line, column)
      else if value.charAt(index) == '\n' then loop(index + 1, line + 1, 0)
      else loop(index + 1, line, column + 1)

    loop(0, baseLine, baseColumn)

  private def indexedOffsetToLineColumn(
    rope: Rope,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
    @tailrec
    def loop(index: Int, line: Int, column: Int): (Int, Int) =
      if index >= offset || index >= rope.weight then (line, column)
      else
        rope.index(index) match
          case Some('\n') => loop(index + 1, line + 1, 0)
          case Some(_)    => loop(index + 1, line, column + 1)
          case None       => (line, column)

    loop(0, baseLine, baseColumn)

object Rope:

  def empty(using balance: Balance): Rope = Leaf("")

  // Normalizes CRLF and bare CR to LF on entry so all downstream code
  // (WrapEngine, RenderEngine, cursor arithmetic) only ever sees '\n'.
  def apply(in: String)(using balance: Balance): Rope =
    build(in.replace("\r\n", "\n").replace("\r", "\n"))

  /** Rebuilds a rope over leaf strings that are already in hand.
    *
    * Oversized leaves are split and adjacent fragments merged, so the leaves stay near the chunk size no matter how
    * ragged the edits that produced them; without that, repeated splitting leaves a tail of one-character leaves and
    * the tree deepens even though it is nominally rebuilt.
    */
  /** F(0) to F(45). A rope deeper than that is degenerate under any criterion, so it is reported unbalanced rather than
    * overflowing the bound.
    */
  private[rope] val minimumWeightsByHeight: Vector[Int] =
    Vector.iterate((0, 1), 46) { case (current, next) => (next, current + next) }.map((current, _) => current)

  private[rope] def fromLeafValues(values: Vector[String])(using balance: Balance): Rope =
    combineBalanced(values.filter(_.nonEmpty))

  /** Splits at the exact character midpoint, cutting the one leaf that straddles it.
    *
    * Splitting on a leaf boundary instead would leave the halves up to a chunk apart, and `isWeightBalanced` is checked
    * after every edit against a threshold far tighter than that. Cutting the straddling leaf keeps the halves within
    * one character while every other leaf passes through untouched, so the characters copied are bounded by the chunk
    * size times the depth rather than by the size of the document.
    */
  private def combineBalanced(values: Vector[String])(using balance: Balance): Rope =
    values match
      case Vector()                                                 => Leaf("")
      case Vector(single) if single.length <= balance.leafChunkSize => Leaf(single)
      case _ =>
        val total = values.foldLeft(0)((running, value) => running + value.length)
        if total <= balance.leafChunkSize then Leaf(values.mkString)
        else
          val (left, right) = splitAtWeight(values, total / 2, Vector.empty)
          Node(combineBalanced(left), combineBalanced(right))

  @tailrec
  private def splitAtWeight(
    values: Vector[String],
    target: Int,
    taken: Vector[String]
  ): (Vector[String], Vector[String]) =
    if values.isEmpty then (taken, Vector.empty)
    else
      val value = values(0)
      if value.length <= target then splitAtWeight(values.drop(1), target - value.length, taken :+ value)
      else if target == 0 then (taken, values)
      else (taken :+ value.take(target), value.drop(target) +: values.drop(1))

  private def build(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(build(left), build(right)).rebalance
