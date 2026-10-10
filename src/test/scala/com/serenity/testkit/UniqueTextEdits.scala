package com.serenity.testkit

import scala.util.Random

import com.serenity.rope.{Balance, ChangeSet, Rope}

/** Single edits to documents of mostly distinct characters, so the edit is the only way to read the difference between
  * the two texts: a diff of before and after reports exactly the region the edit replaced, and a consumer that follows
  * the recorded change has to land where one that compared the texts does.
  */
object UniqueTextEdits:

  given Balance = Balance.default

  final case class Case(before: Rope, after: Rope, change: ChangeSet, lineCount: Int)

  /** `count` edits drawn from `seed`; those whose edit a diff would report differently (an inserted newline beside an
    * existing one, say) are left out, so what comes back is `count` or fewer.
    */
  def cases(seed: Int, count: Int): List[Case] =
    val random = new Random(seed)
    List.fill(count)(drawn(random)).flatten

  private def drawn(random: Random): Option[Case] =
    val length = 1 + random.nextInt(300)
    val text   = documentOf(random, length)
    val from   = random.nextInt(length + 1)
    val to     = math.min(length, from + random.nextInt(8))
    val insert = Iterator.fill(random.nextInt(5))((0x6000 + random.nextInt(0x1000)).toChar).mkString
    Option
      .when(from != to || insert.nonEmpty) {
        val before = Rope(text)
        val change = ChangeSet.single(length, from, to, insert)
        change.applyTo(before).map(after => Case(before, after, change, text.count(_ == '\n') + 1))
      }
      .flatten
      .filter(found => ChangeSet.fromDiff(found.before, found.after) == found.change)

  private def documentOf(random: Random, length: Int): String =
    val distinct = random.shuffle((0 until length).toVector).map(index => (0x4e00 + index).toChar)
    distinct.map(char => if random.nextInt(12) == 0 then '\n' else char).mkString
