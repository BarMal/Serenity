package com.serenity.ui.terminal

import java.awt.Cursor
import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.models.PointerShape

/** Hands `setCursor` a shape only when it differs from the last one handed over. Every frame syncs the pointer shape,
  * but the shape changes only as the pointer crosses between regions; setting an unchanged cursor would post toolkit
  * work on every frame.
  */
final class PointerCursorSync(setCursor: PointerShape => Unit):
  private val lastApplied = AtomicReference[PointerShape](PointerShape.Default)

  def sync(shape: PointerShape): Unit =
    if lastApplied.getAndSet(shape) != shape then setCursor(shape)

object PointerCursorSync:

  def awtCursorType(shape: PointerShape): Int =
    shape match
      case PointerShape.Default          => Cursor.DEFAULT_CURSOR
      case PointerShape.Text             => Cursor.TEXT_CURSOR
      case PointerShape.Hand             => Cursor.HAND_CURSOR
      case PointerShape.ResizeHorizontal => Cursor.E_RESIZE_CURSOR
      case PointerShape.ResizeVertical   => Cursor.N_RESIZE_CURSOR
