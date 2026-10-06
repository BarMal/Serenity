package com.serenity

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import _root_.io.circe.Json
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.session.{SessionManager, SessionState, UnreadableReason, UnreadableSession}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2022: a session that cannot be restored is moved aside, never discarded; its unsaved text is exported as plain
  * text; and the user is told why and where both copies are.
  */
class SessionRestoreNoticeSpec extends AnyFlatSpec with Matchers with OptionValues with StateManagerTestSupport:

  private def sessionManagerAt(root: Path): SessionManager =
    SessionManager.create(
      root,
      AppThemeManager.create,
      testLogger("SessionRestoreNoticeSpec"),
      SessionManager.SessionPolicy.interactive
    )

  private def sessionFile(root: Path): Path =
    root.resolve("sessions").resolve("session.json")

  private def sessionsEntries(root: Path): List[String] =
    val stream = Files.list(root.resolve("sessions"))
    try stream.iterator.asScala.map(_.getFileName.toString).toList.sorted
    finally stream.close()

  private def dirtyState(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    val dirty    = buffer.copy(document = buffer.document.copy(isDirty = true))
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> dirty)))

  /** A real saved session holding `text` as unsaved, then rewritten by `damage`. */
  private def damagedSession(text: String)(damage: String => String): IO[Path] =
    for
      root <- IO.blocking(Files.createTempDirectory("session-restore-notice"))
      _    <- sessionManagerAt(root).saveSession(dirtyState(text))
      _    <- IO.blocking(Files.writeString(sessionFile(root), damage(Files.readString(sessionFile(root)))))
    yield root

  private def fromSchema(version: Int)(json: String): String =
    _root_.io.circe.parser.parse(json).toOption.value.mapObject(_.add("schemaVersion", Json.fromInt(version))).spaces2

  private val newerSchema = SessionState.CurrentSchemaVersion.value + 1

  /** Cut off just after the buffer's content reference, as a write interrupted mid-file would leave it. The text itself
    * lives in a content file beside the session file (#1912), not in the session file.
    */
  private val truncatedAfterContentRef: String => String = json =>
    val reference = "\"contentRef\"\\s*:\\s*\"[0-9a-f]+\"".r
    reference.findFirstMatchIn(json).fold(json)(found => json.take(found.end))

  "SessionManager.setAsideUnreadableCurrentSession" should "move a corrupt session aside and export its unsaved text" in {
    val program = for
      root     <- damagedSession("my unsaved draft")(truncatedAfterContentRef)
      original <- IO.blocking(Files.readString(sessionFile(root)))
      setAside <- sessionManagerAt(root).setAsideUnreadableCurrentSession()
      unreadable = setAside.value
    yield
      unreadable.reason shouldBe UnreadableReason.Corrupt
      Files.exists(sessionFile(root)) shouldBe false
      unreadable.backup.getFileName.toString should startWith("session.json.corrupt-")
      Files.readString(unreadable.backup) shouldBe original
      unreadable.recoveredTexts.map(Files.readString) shouldBe List("my unsaved draft")
      Files.isDirectory(unreadable.backup.resolveSibling(s"${unreadable.backup.getFileName}.content")) shouldBe true
      Files.exists(sessionFile(root).resolveSibling("session.content")) shouldBe false

    program.unsafeRunSync()
  }

  it should "keep a newer-schema session as a .newer backup and export its unsaved text" in {
    val program = for
      root     <- damagedSession("written by a newer build")(fromSchema(newerSchema))
      setAside <- sessionManagerAt(root).setAsideUnreadableCurrentSession()
      unreadable = setAside.value
    yield
      unreadable.reason shouldBe UnreadableReason.NewerVersion(newerSchema)
      unreadable.backup.getFileName.toString should startWith("session.json.newer-")
      Files.exists(sessionFile(root)) shouldBe false
      unreadable.recoveredTexts.map(Files.readString) shouldBe List("written by a newer build")

    program.unsafeRunSync()
  }

  it should "leave a readable session exactly where it is" in {
    val program = for
      root     <- damagedSession("fine")(identity)
      before   <- IO.blocking(sessionsEntries(root))
      setAside <- sessionManagerAt(root).setAsideUnreadableCurrentSession()
      after    <- IO.blocking(sessionsEntries(root))
    yield
      setAside shouldBe None
      after shouldBe before

    program.unsafeRunSync()
  }

  "UnreadableSession.describe" should "say why the session was not restored and where each copy is" in {
    val backup    = Path.of("/home/me/.serenity/sessions/session.json.newer-7")
    val recovered = Path.of("/home/me/.serenity/sessions/session.json.newer-7.recovered/01-notes.md.txt")
    val lines     = UnreadableSession(UnreadableReason.NewerVersion(newerSchema), backup, List(recovered)).describe

    lines.mkString(" ") should include("newer version of Serenity")
    lines.mkString(" ") should include(backup.toString)
    lines.mkString(" ") should include(recovered.getParent.toString)
    UnreadableSession(UnreadableReason.Corrupt, backup, Nil).describe.mkString(" ") should include("damaged")
  }

  private def launch(root: Path, openPath: Option[Path]): IO[AppState] =
    StateManager(
      testLogger("SessionRestoreNoticeSpec"),
      sessionRootOverride = Some(root),
      dictionaryCache = SharedDictionary.default
    ).flatMap { stateManager =>
      AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24),
        openPath = openPath
      )
    }

  private def confirmPrompts(state: AppState): List[ConfirmPrompt] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt }

  "Startup" should "state on the start page and in a prompt that the session could not be restored" in {
    val program = for
      root  <- damagedSession("draft to keep")(fromSchema(newerSchema))
      state <- launch(root, openPath = None)
      startPage = state.startPageSurface.map(_.content).collect { case SurfaceContent.StartPage(page) => page }
      prompt    = confirmPrompts(state).find(_.title == "Session not restored").value
      backups   = sessionsEntries(root).filter(_.startsWith("session.json.newer-"))
    yield
      backups should have size 3 // the backup, its `.content` folder and its `.recovered` folder
      startPage.value.statusMessage.value should include("newer version of Serenity")
      prompt.message.mkString(" ") should include(backups.min)
      prompt.choices.items.map(_.label) should contain("Open 01-untitled-1.txt")

    program.unsafeRunSync()
  }

  it should "still set the session aside and say so when launched with a file to open" in {
    val program = for
      root     <- damagedSession("draft to keep")(truncatedAfterContentRef)
      document <- IO.blocking(Files.writeString(Files.createTempFile("session-restore-open", ".txt"), "opened"))
      state    <- launch(root, openPath = Some(document))
    yield
      Files.exists(sessionFile(root)) shouldBe false
      confirmPrompts(state).map(_.title) should contain("Session not restored")

    program.unsafeRunSync()
  }
