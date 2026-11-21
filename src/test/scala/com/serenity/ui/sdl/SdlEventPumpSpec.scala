package com.serenity.ui.sdl

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pump's event hub, without the native library: what a poll source delivers reaches the stream in order, the
  * window's state still follows its own events, and a reader that is slow or absent cannot hold either up.
  */
class SdlEventPumpSpec extends AnyFlatSpec with Matchers:

  private val start = GlWindowState(pixelWidth = 96, pixelHeight = 64, scale = 1.0)

  private def run[A](io: IO[A]): A = io.timeout(10.seconds).unsafeRunSync()

  private def stamped(event: SdlEvent, at: Long): SdlEvent.Stamped = SdlEvent.Stamped(event, at)

  private def key(code: Int, at: Long): SdlEvent.Stamped =
    stamped(SdlEvent.Key(down = true, repeat = false, scancode = 0, key = code, modifiers = 0), at)

  private def hub: IO[SdlEventHub] = SdlEventHub.create(start)

  private def publishAll(hub: SdlEventHub, events: List[SdlEvent.Stamped]): IO[Unit] =
    events.traverse_(hub.publish(_, 0.0))

  "SdlEventHub" should "hand key, text and mouse events to the stream in the order they were published" in {
    val events = List(
      key(0x61, 1L),
      stamped(SdlEvent.TextInput("a"), 2L),
      stamped(SdlEvent.MouseMotion(3f, 4f, 1f, 1f), 3L),
      stamped(SdlEvent.MouseButton(down = true, button = 1, clicks = 1, x = 3f, y = 4f), 4L)
    )

    val seen = run(hub.flatMap(h => publishAll(h, events) *> h.events.take(events.size.toLong).compile.toList))

    seen shouldBe events
  }

  it should "still fold window events into the window's state as it publishes them" in {
    val (size, afterTake) = run(hub.flatMap { h =>
      publishAll(h, List(stamped(SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, 200, 100), 1L))) *>
        (h.takeResize, h.takeResize).tupled
    })

    size shouldBe Some((200, 100))
    afterTake shouldBe None
  }

  it should "publish the window events as well as folding them" in {
    val resize = stamped(SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, 200, 100), 1L)

    run(hub.flatMap(h => publishAll(h, List(resize)) *> h.events.take(1).compile.toList)) shouldBe List(resize)
  }

  it should "complete awaitClose when a close request is published" in {
    run(hub.flatMap(h => h.publish(stamped(SdlEvent.Quit, 1L), 0.0) *> h.awaitClose)) shouldBe ()
    run(
      hub.flatMap(h =>
        h.publish(stamped(SdlEvent.Window(SdlEvent.WindowKind.CloseRequested, 0, 0), 1L), 0.0) *> h.awaitClose
      )
    ) shouldBe ()
  }

  it should "take the scale it was given when the display scale changes" in {
    val rescaled = stamped(SdlEvent.Window(SdlEvent.WindowKind.DisplayScaleChanged, 0, 0), 1L)

    run(hub.flatMap(h => h.publish(rescaled, 1.5) *> h.state)).scale shouldBe 1.5
  }

  it should "leave the state alone for key and text events" in {
    run(hub.flatMap(h => publishAll(h, List(key(0x61, 1L), stamped(SdlEvent.TextInput("a"), 2L))) *> h.state)) shouldBe
      start
  }

  it should "keep following the window when nobody reads the stream" in {
    val flood = (1L to (SdlEventHub.Backlog + 1000L)).toList.map(at => key(0x61, at))

    run(hub.flatMap(h => publishAll(h, flood :+ stamped(SdlEvent.Quit, 0L)) *> h.awaitClose)) shouldBe ()
  }

  it should "keep the newest events when a reader falls behind" in {
    val total  = SdlEventHub.Backlog + 5
    val flood  = (1L to total.toLong).toList.map(at => key(0x61, at))
    val newest = run(hub.flatMap(h => publishAll(h, flood) *> h.events.take(SdlEventHub.Backlog.toLong).compile.toList))

    newest.map(_.timestampNanos) shouldBe (6L to total.toLong).toList
  }

  private def scripted(batches: List[List[SdlEvent.Stamped]]): IO[IO[(List[SdlEvent.Stamped], Double)]] =
    Ref.of[IO, List[List[SdlEvent.Stamped]]](batches).map { remaining =>
      remaining.modify {
        case next :: rest => (rest, (next, 0.0))
        case Nil          => (Nil, (Nil, 0.0))
      }
    }

  "SdlEventHub.pump" should "deliver what the poll source returns, batch by batch, in order" in {
    val first  = List(key(0x61, 1L), stamped(SdlEvent.TextInput("a"), 2L))
    val second = List(stamped(SdlEvent.MouseMotion(1f, 2f, 0f, 0f), 3L), key(0x62, 4L))

    val seen = run(for
      poll <- scripted(List(first, Nil, second))
      h    <- hub
      seen <- SdlEventHub.pump(poll, h, 1.milli).background.use(_ => h.events.take(4).compile.toList)
    yield seen)

    seen shouldBe first ++ second
  }

  it should "carry on folding the window's state while its events are being read" in {
    val batch = List(
      key(0x61, 1L),
      stamped(SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, 300, 200), 2L),
      stamped(SdlEvent.Window(SdlEvent.WindowKind.CloseRequested, 0, 0), 3L)
    )

    val resize = run(for
      poll   <- scripted(List(batch))
      h      <- hub
      resize <- SdlEventHub.pump(poll, h, 1.milli).background.use(_ => h.awaitClose *> h.takeResize)
    yield resize)

    resize shouldBe Some((300, 200))
  }

  it should "not stall on a stream nobody reads" in {
    val flood = (1L to (SdlEventHub.Backlog + 500L)).toList.map(at => key(0x61, at))

    run(for
      poll <- scripted(List(flood, List(stamped(SdlEvent.Quit, 0L))))
      h    <- hub
      _    <- SdlEventHub.pump(poll, h, 1.milli).background.use(_ => h.awaitClose)
    yield ()) shouldBe ()
  }
