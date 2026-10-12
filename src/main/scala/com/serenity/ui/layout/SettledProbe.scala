package com.serenity.ui.layout

/** A font probe read until it repeats. AWT's first use of a font is not safe against another thread's first use of it:
  * readings taken while two threads warm the same font have been seen to differ from the settled ones (a layout advance
  * that disagrees with the summed glyph advances, a kerning of -0.253 where it is -0.772). A probe is cached for the
  * life of the process, so a reading taken in that window would misjudge the font from then on.
  */
private[layout] object SettledProbe:

  val AgreeingReadings: Int = 3

  val MaxReadings: Int = 50

  def apply[A](reading: => A): A =
    @annotation.tailrec
    def settle(latest: A, agreeing: Int, taken: Int): A =
      if agreeing >= AgreeingReadings || taken >= MaxReadings then latest
      else
        val next = reading
        settle(next, if next == latest then agreeing + 1 else 1, taken + 1)
    settle(reading, 1, 1)
