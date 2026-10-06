package com.serenity.rope

/** A `CharSequence` view of a rope for `java.util.regex`, reading through [[RopeCharacterSource]]'s cached leaf so a
  * forward scan pays one rope descent per leaf rather than per character, and never materialises the whole text.
  */
final class RopeCharSequence private (val source: RopeCharacterSource, start: Int, end: Int) extends CharSequence:

  override def length: Int = end - start

  override def charAt(index: Int): Char = source.charAt(start + index)

  override def subSequence(from: Int, to: Int): CharSequence =
    new RopeCharSequence(source, start + from, start + to)

  override def toString: String = source.rope.sliceString(start, end)

object RopeCharSequence:
  def apply(source: RopeCharacterSource): RopeCharSequence =
    new RopeCharSequence(source, 0, source.length)
