package com.serenity.ui.terminal

import java.awt.Cursor

import com.serenity.state.models.PointerShape
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointerCursorSyncSpec extends AnyFlatSpec with Matchers:

  private def recording(): (PointerCursorSync, scala.collection.mutable.ListBuffer[PointerShape]) =
    val applied = scala.collection.mutable.ListBuffer.empty[PointerShape]
    (new PointerCursorSync(applied += _), applied)

  "PointerCursorSync" should "not touch the cursor while the shape stays the default" in {
    val (sync, applied) = recording()

    sync.sync(PointerShape.Default)

    applied shouldBe empty
  }

  it should "set the cursor once when the shape changes and not again while it holds" in {
    val (sync, applied) = recording()

    sync.sync(PointerShape.Text)
    sync.sync(PointerShape.Text)
    sync.sync(PointerShape.Text)

    applied.toList shouldBe List(PointerShape.Text)
  }

  it should "set the cursor again for each further change, including back to the default" in {
    val (sync, applied) = recording()

    List(PointerShape.Text, PointerShape.Text, PointerShape.ResizeHorizontal, PointerShape.Default).foreach(sync.sync)

    applied.toList shouldBe List(PointerShape.Text, PointerShape.ResizeHorizontal, PointerShape.Default)
  }

  "PointerCursorSync.awtCursorType" should "map every shape to its AWT cursor" in {
    PointerShape.values.map(shape => shape -> PointerCursorSync.awtCursorType(shape)).toMap shouldBe Map(
      PointerShape.Default          -> Cursor.DEFAULT_CURSOR,
      PointerShape.Text             -> Cursor.TEXT_CURSOR,
      PointerShape.Hand             -> Cursor.HAND_CURSOR,
      PointerShape.ResizeHorizontal -> Cursor.E_RESIZE_CURSOR,
      PointerShape.ResizeVertical   -> Cursor.N_RESIZE_CURSOR
    )
  }
