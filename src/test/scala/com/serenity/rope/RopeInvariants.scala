package com.serenity.rope

import com.serenity.rope.btree.{Fanout, LeafLimits}
import com.serenity.text.TextCounts

/** What must hold of the B-tree behind any rope (#1900), as a list of the ways it does not.
  *
  *   - every leaf is at the same depth, and `height` says how deep;
  *   - a branch has `Fanout.Min` to `Fanout.Max` children, the root two or more;
  *   - a leaf holds at most the largest leaf size and, unless it is the whole tree, at least the smallest;
  *   - every cached figure -- offsets, newline counts, last-line length, trailing newline, word and character counts --
  *     equals the figure recomputed from the text beneath the node.
  *
  * A leaf lower bound can be missed by one unit when the only cut point available would split a surrogate pair, so
  * specs checking it use text without surrogates. Counts are compared only when asked: word segmentation is exact for
  * ordinary text but is documented as approximate for a few scripts.
  */
object RopeInvariants:

  final private case class Shape(violations: List[String], leafDepths: Set[Int])

  final private case class Check(limits: LeafLimits, figures: Boolean, counts: Boolean, leafMinimum: Boolean)

  /** `checkFigures` recomputes every cached figure from the text beneath each node, which costs the text length times
    * the height; without it only the shape is checked. `checkCounts` adds the word and character counts to that.
    * `checkLeafMinimum` is off for ropes built from arbitrary given leaves, which may be as small as they like.
    */
  def violations(
    rope: Rope,
    checkFigures: Boolean = true,
    checkCounts: Boolean = true,
    checkLeafMinimum: Boolean = true
  )(using balance: Balance): List[String] =
    inspect(
      rope,
      Check(LeafLimits.of(balance), checkFigures, checkCounts, checkLeafMinimum),
      isRoot = true,
      "root"
    ).violations

  /** The leaf strings left to right. */
  def leaves(rope: Rope): Vector[String] = RopeChunks.of(rope).toVector

  private def inspect(rope: Rope, check: Check, isRoot: Boolean, path: String): Shape =
    rope match
      case leaf: Leaf =>
        val size   = leaf.value.length
        val limits = check.limits
        val found = List(
          Option.when(size > limits.max)(s"$path: leaf of $size units exceeds ${limits.max}"),
          Option.when(check.leafMinimum && !isRoot && size < limits.min)(
            s"$path: leaf of $size units is under ${limits.min}"
          )
        ).flatten
        Shape(found ++ cachedFigures(leaf, path, check), Set(1))
      case branch: Branch => inspectBranch(branch, check, isRoot, path)

  private def inspectBranch(branch: Branch, check: Check, isRoot: Boolean, path: String): Shape =
    val children = branch.children
    val shapes   = children.zipWithIndex.map((child, index) => inspect(child, check, isRoot = false, s"$path/$index"))
    val depths   = shapes.flatMap(_.leafDepths).toSet
    val found = List(
      Option.when(children.size > Fanout.Max)(s"$path: ${children.size} children exceeds ${Fanout.Max}"),
      Option.when(!isRoot && children.size < Fanout.Min)(s"$path: ${children.size} children is under ${Fanout.Min}"),
      Option.when(isRoot && children.size < 2)(s"$path: a root branch with ${children.size} children"),
      Option.when(depths.size > 1)(s"$path: leaves at depths ${depths.toList.sorted.mkString(", ")}"),
      Option.when(depths.size == 1 && depths.map(_ + 1) != Set(branch.height))(
        s"$path: height ${branch.height} but leaves ${depths.toList.sorted.mkString(", ")} below"
      ),
      Option.when(!branch.starts.toVector.sameElements(offsetsOf(children)))(s"$path: stale child offsets"),
      Option.when(!branch.newlines.toVector.sameElements(newlinesOf(children)))(s"$path: stale newline counts")
    ).flatten
    Shape(
      shapes.flatMap(_.violations).toList ++ found ++ cachedFigures(branch, path, check),
      depths.map(_ + 1)
    )

  private def offsetsOf(children: Vector[Rope]): Vector[Int] =
    children.scanLeft(0)((offset, child) => offset + child.weight)

  private def newlinesOf(children: Vector[Rope]): Vector[Int] =
    children.scanLeft(0)((count, child) => count + child.newlineCount)

  private def cachedFigures(node: Rope, path: String, check: Check): List[String] =
    if !check.figures then Nil else figuresAgainstText(node, path, check.counts)

  private def figuresAgainstText(node: Rope, path: String, checkCounts: Boolean): List[String] =
    val text   = node.collect()
    val counts = if checkCounts then Some(TextCounts.of(text)) else None
    List(
      Option.when(node.weight != text.length)(s"$path: weight ${node.weight} but ${text.length} units beneath"),
      Option.when(node.newlineCount != text.count(_ == '\n'))(s"$path: stale newline count ${node.newlineCount}"),
      Option.when(node.lastLineLength != text.length - text.lastIndexOf('\n') - 1)(
        s"$path: stale last line length ${node.lastLineLength}"
      ),
      Option.when(node.endsWithNewline != text.endsWith("\n"))(s"$path: stale trailing newline flag"),
      counts
        .filter(expected => node.textSummary.counts != expected)
        .map(expected => s"$path: counts ${node.textSummary.counts} but the text has $expected")
    ).flatten
