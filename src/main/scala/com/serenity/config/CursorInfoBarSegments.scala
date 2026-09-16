package com.serenity.config

/** Pure ordering rules for the cursor info bar's segment list -- what toggling a segment on or nudging it earlier or
  * later does to the persisted order. Kept out of the effect layer so the rules can be tested as plain functions.
  */
object CursorInfoBarSegments:

  /** Adds `segment` at its slot in the canonical definition order (`CursorInfoBarSegment.values`) relative to the
    * already-included segments, rather than appending to the end. So toggling a segment off then on restores its
    * position instead of shunting it to the tail (#1533); existing segments keep their relative order, so a user's
    * manual reordering via move-earlier/later is preserved. A no-op when the segment is already present.
    */
  def include(
    segments: List[CursorInfoBarSegment],
    segment: CursorInfoBarSegment
  ): List[CursorInfoBarSegment] =
    if segments.contains(segment) then segments
    else
      val canonicalOrder                          = CursorInfoBarSegment.values.toList
      def canonicalIndex(s: CursorInfoBarSegment) = canonicalOrder.indexOf(s)
      segments.indexWhere(existing => canonicalIndex(existing) > canonicalIndex(segment)) match
        case -1       => segments :+ segment
        case insertAt => segments.patch(insertAt, List(segment), 0)

  def move(
    segments: List[CursorInfoBarSegment],
    segment: CursorInfoBarSegment,
    delta: Int
  ): List[CursorInfoBarSegment] =
    val index  = segments.indexOf(segment)
    val target = index + delta
    if index < 0 || target < 0 || target >= segments.length then segments
    else
      segments.zipWithIndex.map {
        case (_, `index`)  => segments(target)
        case (_, `target`) => segments(index)
        case (other, _)    => other
      }
