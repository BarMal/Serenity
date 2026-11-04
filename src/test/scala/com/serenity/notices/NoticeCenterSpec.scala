package com.serenity.notices

import scala.concurrent.duration.*

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The notice centre (#2523 step 3): what is raised, what is still unseen, and when an error may toast. All times are
  * monotonic nanoseconds handed in by the test.
  */
class NoticeCenterSpec extends AnyFlatSpec with Matchers with OptionValues:

  private def at(seconds: Long): Long = seconds * 1_000_000_000L

  private def key(code: String, subject: NoticeSubject = NoticeSubject.Language("scala")): NoticeKey =
    NoticeKey(NoticeSource.Lsp, subject, code)

  private def condition(code: String, severity: NoticeSeverity, message: String = "m"): NoticeSpec =
    NoticeSpec(key(code), NoticeKind.Condition, severity, message)

  private def event(code: String, severity: NoticeSeverity, message: String = "m"): NoticeSpec =
    NoticeSpec(key(code), NoticeKind.Event, severity, message)

  private def center: NoticeCenter = NoticeCenter.empty

  "An empty centre" should "hold nothing and count nothing" in {
    NoticeCenter.empty.entries shouldBe empty
    NoticeCenter.empty.unseen shouldBe NoticeCounts()
  }

  "Raising a notice" should "store it once, counted as unseen at its severity" in {
    val raised = center.raise(condition("a", NoticeSeverity.Warning), at(1))

    raised.center.entries should have size 1
    raised.center.entries.get(key("a")).value.count shouldBe 1
    raised.center.unseen shouldBe NoticeCounts(warning = 1)
  }

  it should "toast an error but never a warning or an info" in {
    val error   = center.raise(event("e", NoticeSeverity.Error), at(1))
    val warning = center.raise(event("w", NoticeSeverity.Warning), at(1))
    val info    = center.raise(event("i", NoticeSeverity.Info), at(1))

    error.toast shouldBe true
    warning.toast shouldBe false
    info.toast shouldBe false
  }

  "Raising a notice that is already there" should "coalesce into one entry with a count and the latest words" in {
    val first  = center.raise(condition("a", NoticeSeverity.Warning, "first"), at(1))
    val second = first.center.raise(condition("a", NoticeSeverity.Warning, "second"), at(5))

    val entry = second.center.entries.get(key("a")).value
    entry.count shouldBe 2
    entry.message shouldBe "second"
    entry.firstAt shouldBe at(1)
    entry.lastAt shouldBe at(5)
    second.center.unseen shouldBe NoticeCounts(warning = 1)
  }

  it should "not toast again when nothing about it changed" in {
    val first  = center.raise(condition("a", NoticeSeverity.Error), at(1))
    val second = first.center.raise(condition("a", NoticeSeverity.Error), at(2))

    first.toast shouldBe true
    second.toast shouldBe false
    second.center.entries.get(key("a")).value.count shouldBe 2
  }

  it should "toast again when an error's words change" in {
    val first  = center.raise(condition("a", NoticeSeverity.Error, "one"), at(1))
    val second = first.center.raise(condition("a", NoticeSeverity.Error, "two"), at(2))

    second.toast shouldBe true
    second.center.entries.get(key("a")).value.message shouldBe "two"
  }

  it should "toast when a warning escalates to an error, and move the count with it" in {
    val warned    = center.raise(condition("a", NoticeSeverity.Warning), at(1))
    val escalated = warned.center.raise(condition("a", NoticeSeverity.Error), at(2))

    escalated.toast shouldBe true
    escalated.center.unseen shouldBe NoticeCounts(error = 1)
  }

  "Acknowledging a notice" should "take it out of the unseen count and keep it listed" in {
    val raised = center.raise(condition("a", NoticeSeverity.Error), at(1)).center

    val acknowledged = raised.acknowledge(key("a"))

    acknowledged.unseen shouldBe NoticeCounts()
    acknowledged.entries.get(key("a")).value.acknowledged shouldBe true
  }

  it should "stay acknowledged when it is raised again unchanged, and be unseen again when it changes" in {
    val acknowledged = center.raise(condition("a", NoticeSeverity.Warning, "x"), at(1)).center.acknowledge(key("a"))

    val unchanged = acknowledged.raise(condition("a", NoticeSeverity.Warning, "x"), at(2))
    val changed   = acknowledged.raise(condition("a", NoticeSeverity.Warning, "y"), at(2))

    unchanged.center.unseen shouldBe NoticeCounts()
    unchanged.toast shouldBe false
    changed.center.unseen shouldBe NoticeCounts(warning = 1)
  }

  "Acknowledging everything" should "leave nothing unseen" in {
    val raised = center
      .raise(event("a", NoticeSeverity.Info), at(1))
      .center
      .raise(condition("b", NoticeSeverity.Error), at(1))
      .center

    raised.unseen.total shouldBe 2
    raised.acknowledgeAll.unseen shouldBe NoticeCounts()
  }

  "Resolving a condition" should "take it out of the unseen count and mark it resolved" in {
    val raised = center.raise(condition("a", NoticeSeverity.Error), at(1)).center

    val resolved = raised.resolve(key("a"), at(2))

    resolved.unseen shouldBe NoticeCounts()
    resolved.entries.get(key("a")).value.resolvedAt shouldBe Some(at(2))
  }

  it should "leave an event alone, since an event is not an ongoing condition" in {
    val raised = center.raise(event("a", NoticeSeverity.Error), at(1)).center

    raised.resolve(key("a"), at(2)) shouldBe raised
    raised.resolve(key("nothing"), at(2)) shouldBe raised
  }

  it should "ignore a resolution observed before the latest raise" in {
    val raised = center.raise(condition("a", NoticeSeverity.Error), at(10)).center

    val resolved = raised.resolve(key("a"), at(5))

    resolved shouldBe raised
    resolved.unseen shouldBe NoticeCounts(error = 1)
  }

  "Raising a condition that was resolved" should "ignore a raise observed before the resolution" in {
    val resolved = center.raise(condition("a", NoticeSeverity.Error), at(1)).center.resolve(key("a"), at(10))

    val stale = resolved.raise(condition("a", NoticeSeverity.Error), at(5))

    stale.center shouldBe resolved
    stale.toast shouldBe false
  }

  it should "reopen quietly inside the cooldown, keeping the count and what the user acknowledged" in {
    val resolved = center
      .raise(condition("a", NoticeSeverity.Error), at(1))
      .center
      .acknowledge(key("a"))
      .resolve(key("a"), at(10))

    val flapped = resolved.raise(condition("a", NoticeSeverity.Error), at(20))

    val entry = flapped.center.entries.get(key("a")).value
    flapped.toast shouldBe false
    entry.resolvedAt shouldBe None
    entry.count shouldBe 2
    entry.acknowledged shouldBe true
    flapped.center.unseen shouldBe NoticeCounts()
  }

  it should "start a new episode after the cooldown: count one, unseen, and toasting" in {
    val resolved = center
      .raise(condition("a", NoticeSeverity.Error), at(1))
      .center
      .acknowledge(key("a"))
      .resolve(key("a"), at(10))

    val again = resolved.raise(condition("a", NoticeSeverity.Error), at(10 + 61))

    val entry = again.center.entries.get(key("a")).value
    again.toast shouldBe true
    entry.count shouldBe 1
    entry.firstAt shouldBe at(71)
    entry.acknowledged shouldBe false
    again.center.unseen shouldBe NoticeCounts(error = 1)
  }

  "Toasting" should "allow a burst of three errors, refuse the fourth, and allow one more after the interval" in {
    val burst = (1 to 4).foldLeft((center, List.empty[Boolean])) { case ((current, toasts), n) =>
      val raised = current.raise(event(s"e$n", NoticeSeverity.Error), at(100))
      (raised.center, toasts :+ raised.toast)
    }
    val later = burst._1.raise(event("e5", NoticeSeverity.Error), at(100 + 20))

    burst._2 shouldBe List(true, true, true, false)
    later.toast shouldBe true
  }

  it should "still count an error whose toast was refused" in {
    val flooded = (1 to 5).foldLeft(center)((current, n) =>
      current.raise(event(s"e$n", NoticeSeverity.Error), at(100)).center
    )

    flooded.entries should have size 5
    flooded.unseen shouldBe NoticeCounts(error = 5)
  }

  private val small = NoticeCenter(policy = NoticePolicy(maxEntries = 3))

  "A full centre" should "make room by dropping a resolved condition first" in {
    val full = small
      .raise(condition("resolved", NoticeSeverity.Warning), at(1))
      .center
      .resolve(key("resolved"), at(2))
      .raise(event("event", NoticeSeverity.Warning), at(3))
      .center
      .raise(condition("active", NoticeSeverity.Warning), at(4))
      .center

    val next = full.raise(event("new", NoticeSeverity.Warning), at(5)).center

    next.entries.keySet shouldBe Set(key("event"), key("active"), key("new"))
  }

  it should "then drop an acknowledged event, then the oldest event" in {
    val full = small
      .raise(event("old", NoticeSeverity.Warning), at(1))
      .center
      .raise(event("acked", NoticeSeverity.Warning), at(2))
      .center
      .acknowledge(key("acked"))
      .raise(event("newer", NoticeSeverity.Warning), at(3))
      .center

    val first  = full.raise(event("x", NoticeSeverity.Warning), at(4)).center
    val second = first.raise(event("y", NoticeSeverity.Warning), at(5)).center

    first.entries.keySet shouldBe Set(key("old"), key("newer"), key("x"))
    second.entries.keySet shouldBe Set(key("newer"), key("x"), key("y"))
  }

  it should "never make an event push out an active condition, and say it dropped the event" in {
    val full = (1 to 3).foldLeft(small)((current, n) =>
      current.raise(condition(s"c$n", NoticeSeverity.Warning), at(n.toLong)).center
    )

    val next = full.raise(event("flood", NoticeSeverity.Warning), at(10))

    next.center.entries.keySet shouldBe full.entries.keySet
    next.center.dropped shouldBe 1L
    next.toast shouldBe false
  }

  it should "let a new condition push out the oldest acknowledged condition, then the oldest" in {
    val full = (1 to 3)
      .foldLeft(small)((current, n) => current.raise(condition(s"c$n", NoticeSeverity.Warning), at(n.toLong)).center)
      .acknowledge(key("c2"))

    val first  = full.raise(condition("new1", NoticeSeverity.Warning), at(10)).center
    val second = first.raise(condition("new2", NoticeSeverity.Warning), at(11)).center

    first.entries.keySet shouldBe Set(key("c1"), key("c3"), key("new1"))
    second.entries.keySet shouldBe Set(key("c3"), key("new1"), key("new2"))
  }

  "Clearing what is inactive" should "remove resolved conditions and acknowledged events and nothing else" in {
    val mixed = center
      .raise(condition("resolved", NoticeSeverity.Warning), at(1))
      .center
      .resolve(key("resolved"), at(2))
      .raise(event("acked", NoticeSeverity.Warning), at(3))
      .center
      .acknowledge(key("acked"))
      .raise(event("unseen", NoticeSeverity.Warning), at(4))
      .center
      .raise(condition("active", NoticeSeverity.Warning), at(5))
      .center

    mixed.clearInactive.entries.keySet shouldBe Set(key("unseen"), key("active"))
  }

  "A storm of one notice" should "leave one entry with the whole count and a single toast" in {
    val storm = (1 to 100_000).foldLeft((center, 0)) { case ((current, toasts), n) =>
      val raised = current.raise(condition("storm", NoticeSeverity.Error), at(1) + n.toLong)
      (raised.center, toasts + (if raised.toast then 1 else 0))
    }

    storm._1.entries should have size 1
    storm._1.entries.get(key("storm")).value.count shouldBe 100_000
    storm._2 shouldBe 1
  }

  "A flood of distinct events" should "stay within the limit and leave an active condition where it was" in {
    val withCondition = center.raise(condition("keep", NoticeSeverity.Error), at(1)).center
    val flooded = (1 to 10_000).foldLeft(withCondition)((current, n) =>
      current.raise(event(s"flood$n", NoticeSeverity.Warning), at(2) + n.toLong).center
    )

    flooded.entries.size should be <= NoticePolicy().maxEntries
    flooded.entries.get(key("keep")).value.resolvedAt shouldBe None
  }

  "The unseen count" should "always equal a recount of the entries, whatever was done" in {
    val keys = (0 until 12).map(n => key(s"k$n", NoticeSubject.Buffer(n))).toVector

    def next(seed: Long): Long = seed * 6364136223846793005L + 1442695040888963407L

    def step(state: (NoticeCenter, Long, Long)): (NoticeCenter, Long, Long) =
      val (current, seed, now) = state
      val random               = next(seed)
      val pick                 = (random >>> 33).toInt.abs
      val target               = keys(pick % keys.size)
      val severity             = NoticeSeverity.fromOrdinal((pick / 7) % 3)
      val parity               = (pick / 3) % 2
      val kind                 = if parity == 0 then NoticeKind.Condition else NoticeKind.Event
      val later                = now + at(1) * ((pick / 5) % 40)
      val updated = (pick / 11) % 6 match
        case 0 => current.resolve(target, later)
        case 1 => current.acknowledge(target)
        case 2 => current.acknowledgeAll
        case 3 => current.clearInactive
        case _ => current.raise(NoticeSpec(target, kind, severity, s"m${pick % 3}"), later).center
      (updated, random, later)

    val states = Iterator.iterate((NoticeCenter(policy = NoticePolicy(maxEntries = 8)), 42L, at(1)))(step).take(5_000)

    states.map(_._1).forall(c => c.unseen == c.recounted) shouldBe true
  }
