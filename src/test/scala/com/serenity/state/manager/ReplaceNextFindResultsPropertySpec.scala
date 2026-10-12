package com.serenity.state.manager

import com.serenity.rope.{Balance, Rope, isWholeGraphemeRange}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** After replace-next, the prompt's find results and the text must be exactly what a fresh, naive search of the
  * replaced text gives: the same matches, the same "next" match (wrapping to the first), over documents that include
  * combining marks and surrogate pairs, where a match may begin or end inside a grapheme.
  */
class ReplaceNextFindResultsPropertySpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with OptionValues:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 8)

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)

  private val genPiece: Gen[String]          = Gen.oneOf("a", "b", "ab", " ", "\n", "é", "́", "🙂")
  private def genText(max: Int): Gen[String] = Gen.choose(0, max).flatMap(n => Gen.listOfN(n, genPiece).map(_.mkString))

  private def genFind(text: String): Gen[String] =
    val invented = Gen.choose(1, 3).flatMap(n => Gen.listOfN(n, genPiece).map(_.mkString)).suchThat(_.nonEmpty)
    if text.isEmpty then invented
    else
      val taken = for
        from   <- Gen.choose(0, text.length - 1)
        length <- Gen.choose(1, 3)
      yield text.slice(from, from + length)
      Gen.frequency(3 -> taken, 1 -> invented)

  private val genCase: Gen[(String, String, String, Int)] =
    for
      text        <- genText(40)
      find        <- genFind(text)
      replacement <- genText(3)
      cursor      <- Gen.choose(0, text.length)
    yield (text, find, replacement, cursor)

  private def occurrences(text: String, find: String): List[Int] =
    LazyList
      .iterate(text.indexOf(find))(found => text.indexOf(find, found + find.length))
      .takeWhile(_ >= 0)
      .toList

  private def wholeGrapheme(text: String, find: String): List[Int] =
    val rope = Rope(text)
    occurrences(text, find).filter(offset => rope.isWholeGraphemeRange(offset, offset + find.length))

  private def position(text: String, offset: Int): FindResult =
    val before = text.take(offset)
    FindResult(before.count(_ == '\n'), offset - (before.lastIndexOf('\n') + 1))

  private def modelWith(text: String, find: String, replacement: String, cursor: Int): Model =
    val cursorPosition = position(text, cursor)
    val buffer = Buffer(
      bufferId,
      Document(content = Rope(text)),
      editing = EditingState.fromCursors(List(Cursor(CursorPosition(cursorPosition.line, cursorPosition.column))))
    )
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val workflow = ReplaceWorkflowState(
      findText = find,
      replacementText = replacement,
      selectedAction = ReplaceWorkflowAction.ReplaceNext
    )
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    Model(base.copy(runtime = base.runtime.copy(uiSurfaces = List(surface))), UndoState())

  private def checkReplaceNext(text: String, find: String, replacement: String, cursor: Int): Unit =
    val matches = wholeGrapheme(text, find)
    matches should not be empty
    val start    = matches.find(_ >= cursor).getOrElse(matches.head)
    val expected = text.take(start) + replacement + text.drop(start + find.length)

    val after  = ReplaceWorkflowTransitions.submitted(modelWith(text, find, replacement, cursor), surfaceId).value
    val buffer = after.app.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe expected

    val fresh = wholeGrapheme(expected, find)
    buffer.findState.map(_.results.toList) shouldBe Option.when(fresh.nonEmpty)(fresh.map(position(expected, _)))
    val nextIndex = fresh.indexWhere(_ >= start + replacement.length) match
      case -1    => 0
      case index => index
    buffer.findState.map(_.currentIndex) shouldBe Option.when(fresh.nonEmpty)(nextIndex)

  property("replace-next counts non-overlapping matches, as the editor's search does: bbbbb, bb -> bbb") {
    checkReplaceNext("bbbbb", "bb", "", 0)
  }

  property("replace-next leaves exactly the matches and next match of a fresh search of the new text") {
    forAll(genCase) { (text, find, replacement, cursor) =>
      whenever(wholeGrapheme(text, find).nonEmpty) {
        checkReplaceNext(text, find, replacement, cursor)
      }
    }
  }
