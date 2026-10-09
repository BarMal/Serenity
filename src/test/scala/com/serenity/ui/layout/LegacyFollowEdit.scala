package com.serenity.ui.layout

import scala.annotation.tailrec

import com.serenity.rope.{Leaf, Node, Rope}

/** The leaf-vector diff `VisualLineIndexStore.followEdit` used before it walked both ropes in step, kept verbatim as
  * the oracle the walk must agree with on every input.
  */
object LegacyFollowEdit:

  def followEdit(index: VisualLineIndex, before: Rope, after: Rope): VisualLineIndex =
    if index.lineCount != before.lineCount then VisualLineIndex.unmeasured(after.lineCount)
    else
      val beforeLeaves = leafStrings(before)
      val afterLeaves  = leafStrings(after)
      val bound        = math.min(before.weight, after.weight)
      val prefix       = commonPrefix(beforeLeaves, afterLeaves, bound)
      if prefix == bound && before.weight == after.weight then index
      else
        val suffix          = commonPrefix(beforeLeaves.reverse, afterLeaves.reverse, bound - prefix, fromEnd = true)
        val (firstLine, _)  = after.offsetToLineColumn(prefix)
        val (lastBefore, _) = before.offsetToLineColumn(before.weight - suffix)
        val (lastAfter, _)  = after.offsetToLineColumn(after.weight - suffix)
        val followed        = index.replacedLines(firstLine, lastBefore - firstLine + 1, lastAfter - firstLine + 1)
        if followed.lineCount == after.lineCount then followed else VisualLineIndex.unmeasured(after.lineCount)

  private def leafStrings(rope: Rope): Vector[String] =
    @tailrec
    def collect(pending: List[Rope], acc: Vector[String]): Vector[String] =
      pending match
        case Node(left, right) :: rest            => collect(left :: right :: rest, acc)
        case Leaf(value) :: rest if value.isEmpty => collect(rest, acc)
        case Leaf(value) :: rest                  => collect(rest, acc :+ value)
        case Nil                                  => acc
    collect(List(rope), Vector.empty)

  private def commonPrefix(a: Vector[String], b: Vector[String], bound: Int, fromEnd: Boolean = false): Int =
    def charAt(leaf: String, offset: Int): Char = leaf.charAt(if fromEnd then leaf.length - 1 - offset else offset)
    @tailrec
    def sharedLeaves(index: Int, shared: Int): (Int, Int) =
      if index < a.length && index < b.length && (a(index) eq b(index)) && shared + a(index).length <= bound then
        sharedLeaves(index + 1, shared + a(index).length)
      else (index, shared)
    @tailrec
    def sharedChars(aLeaf: Int, aOffset: Int, bLeaf: Int, bOffset: Int, shared: Int): Int =
      if shared >= bound || aLeaf >= a.length || bLeaf >= b.length then shared
      else if aOffset >= a(aLeaf).length then sharedChars(aLeaf + 1, 0, bLeaf, bOffset, shared)
      else if bOffset >= b(bLeaf).length then sharedChars(aLeaf, aOffset, bLeaf + 1, 0, shared)
      else if charAt(a(aLeaf), aOffset) != charAt(b(bLeaf), bOffset) then shared
      else sharedChars(aLeaf, aOffset + 1, bLeaf, bOffset + 1, shared + 1)
    val (leaf, shared) = sharedLeaves(0, 0)
    sharedChars(leaf, 0, leaf, 0, shared)
