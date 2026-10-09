package com.serenity.state.reducers

import java.util.{Collections, IdentityHashMap}

import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.{Balance, Leaf, Node, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What an undo step costs in memory (#1930). Each step holds the whole pre-edit buffer, but its text is a persistent
  * `Rope`, so it shares every subtree the edit did not touch with the version after it. The steps therefore add nodes
  * in proportion to the depth of the tree -- logarithmic in the document -- not to the document, which is what storing
  * only a change set would buy. These specs count the distinct rope nodes a history keeps alive.
  */
class UndoSnapshotSharingSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)
  private val edits    = 200

  private def nodesOf(ropes: Seq[Rope]): Int =
    val seen = Collections.newSetFromMap(new IdentityHashMap[Rope, java.lang.Boolean]())
    def visit(rope: Rope): Unit =
      if seen.add(rope) then
        rope match
          case Node(left, right) =>
            visit(left)
            visit(right)
          case _: Leaf => ()
    ropes.foreach(visit)
    seen.size

  /** A `lines`-line document, typed into `edits` times at its midpoint: the rope before every edit, then after all. */
  private def ropesAcrossTypingAtMiddle(lines: Int): Seq[Rope] =
    val text =
      Vector.tabulate(lines)(line => s"line $line of the manuscript, kept long enough to fill a row.").mkString("\n")
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(List(CursorPosition(lines / 2, 0))))
    val start  = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val (last, snapshotRopes) = (1 to edits).foldLeft((start, Vector.empty[Rope])) {
      case ((state, ropes), _) =>
        val result = EditorEventReducer.reduce(InsertChar('x'), paneId, state)
        val before = result.effects.collectFirst {
          case AppEffect.Undo(UndoEffect.RecordBoundary(edit: HistoryEntry.BufferEdit, _)) => edit.snapshot.content
        }
        (result.state, ropes ++ before)
    }
    snapshotRopes :+ last.persisted.buffers(bufferId).document.content

  private def retainedPerEdit(lines: Int): (Int, Int) =
    val ropes = ropesAcrossTypingAtMiddle(lines)
    val base  = nodesOf(ropes.take(1))
    ((nodesOf(ropes) - base) / edits, base)

  "A history of edits" should "add nodes in proportion to the tree's depth, not the document's size" in {
    val (smallPerEdit, smallBase) = retainedPerEdit(lines = 1000)
    val (largePerEdit, largeBase) = retainedPerEdit(lines = 16000)

    largeBase should be > smallBase * 8
    largePerEdit should be < smallPerEdit * 2
    largePerEdit should be < largeBase / 50
  }

  it should "share almost every node with the version it came from" in {
    val ropes = ropesAcrossTypingAtMiddle(lines = 16000)

    nodesOf(ropes) should be < nodesOf(ropes.take(1)) + edits * 40
  }
