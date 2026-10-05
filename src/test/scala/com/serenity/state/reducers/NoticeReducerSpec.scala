package com.serenity.state.reducers

import scala.concurrent.duration.*

import com.serenity.config.CornerPosition
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.PeekContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The notice surface (#1717): a corner message that never takes focus, replaces an older one on its own topic, and
  * leaves by itself only when it is not an error.
  */
class NoticeReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val start = 1_000_000_000L

  private def saveFailed(bufferId: Int, message: String): Notice =
    Notice(NoticeLevel.Error, message, topic = Some(NoticeTopic.FileSave(BufferId(bufferId))))

  private val restored = Notice(NoticeLevel.Info, "Session restored.")
  private val warning  = Notice(NoticeLevel.Warning, "Couldn't back up this session: permission denied.")

  "Showing a notice" should "float it in the bottom-right corner without moving focus" in {
    val state = AppState.initial
    val error = saveFailed(1, "Couldn't save notes.md: permission denied.")

    val shown = NoticeReducer.shown(state, error, start)

    NoticeReducer.visible(shown) shouldBe List(error)
    shown.persisted.focus shouldBe state.persisted.focus
    val surface = shown.runtime.uiSurfaces.lastOption.getOrElse(fail("no surface was added"))
    surface.presentation shouldBe SurfacePresentation.Floating(
      None,
      SurfacePlacement.Corner(CornerPosition.BottomRight)
    )
    surface.focusPolicy shouldBe SurfaceFocusPolicy.Peek
    surface.isFloatingPeek shouldBe false
  }

  it should "replace an older notice on the same topic" in {
    val first  = saveFailed(1, "Couldn't save notes.md: permission denied.")
    val second = saveFailed(1, "Couldn't save notes.md: the disk is full.")

    val shown = NoticeReducer.shown(NoticeReducer.shown(AppState.initial, first, start), second, start + 1)

    NoticeReducer.visible(shown) shouldBe List(second)
  }

  it should "keep notices on other topics, oldest first" in {
    val notes = saveFailed(1, "Couldn't save notes.md: permission denied.")
    val todo  = saveFailed(2, "Couldn't save todo.md: permission denied.")

    val shown = NoticeReducer.shown(NoticeReducer.shown(AppState.initial, notes, start), todo, start + 1)

    NoticeReducer.visible(shown) shouldBe List(notes, todo)
  }

  it should "show at most three at once, dropping the oldest" in {
    val notices = (1 to 4).toList.map(id => saveFailed(id, s"Couldn't save file$id.md: permission denied."))

    val shown = notices.foldLeft(AppState.initial)((state, notice) => NoticeReducer.shown(state, notice, start))

    NoticeReducer.visible(shown) shouldBe notices.drop(1)
  }

  it should "survive a peek opening, which replaces only other peeks" in {
    val error = saveFailed(1, "Couldn't save notes.md: permission denied.")
    val shown = NoticeReducer.shown(AppState.initial, error, start)

    val peeked = PeekStateReducer.show(PeekContent.QuickInfo("hover"), CursorPosition(0, 0), shown).state

    NoticeReducer.visible(peeked) shouldBe List(error)
  }

  "An info notice" should "stay until its time is up, then leave" in {
    val shown    = NoticeReducer.shown(AppState.initial, restored, start)
    val lifetime = NoticeLevel.Info.autoDismissAfter.getOrElse(fail("info notices should dismiss themselves"))

    NoticeReducer.visible(NoticeReducer.expired(shown, start + lifetime.toNanos - 1)) shouldBe List(restored)
    NoticeReducer.visible(NoticeReducer.expired(shown, start + lifetime.toNanos)) shouldBe empty
  }

  "A warning" should "leave by itself, later than an info notice" in {
    val warningLifetime = NoticeLevel.Warning.autoDismissAfter.getOrElse(fail("warnings should dismiss themselves"))
    val infoLifetime    = NoticeLevel.Info.autoDismissAfter.getOrElse(fail("info notices should dismiss themselves"))

    warningLifetime should be > infoLifetime
    val shown = NoticeReducer.shown(AppState.initial, warning, start)
    NoticeReducer.visible(NoticeReducer.expired(shown, start + warningLifetime.toNanos)) shouldBe empty
  }

  "An error notice" should "stay until it is dismissed" in {
    val error = saveFailed(1, "Couldn't save notes.md: permission denied.")
    val shown = NoticeReducer.shown(AppState.initial, error, start)

    NoticeLevel.Error.autoDismissAfter shouldBe None
    NoticeReducer.visible(NoticeReducer.expired(shown, start + 1.day.toNanos)) shouldBe List(error)
  }

  "Sweeping expired notices" should "leave the state untouched when nothing has expired" in {
    val shown = NoticeReducer.shown(AppState.initial, restored, start)

    NoticeReducer.expired(shown, start) should be theSameInstanceAs shown
  }

  "Clearing a topic" should "remove only that topic's notice" in {
    val notes = saveFailed(1, "Couldn't save notes.md: permission denied.")
    val shown = NoticeReducer.shown(NoticeReducer.shown(AppState.initial, notes, start), warning, start)

    val cleared = NoticeReducer.withoutTopic(shown, NoticeTopic.FileSave(BufferId(1)))

    NoticeReducer.visible(cleared) shouldBe List(warning)
    NoticeReducer.withoutTopic(cleared, NoticeTopic.FileSave(BufferId(1))) should be theSameInstanceAs cleared
  }
