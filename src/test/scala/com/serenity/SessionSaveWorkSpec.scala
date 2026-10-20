package com.serenity

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.{FileStamp, SettledClock}
import com.serenity.richtext.RichTextDocument
import com.serenity.rope.{Balance, Leaf}
import com.serenity.session.{SessionBuffer, SessionManager}
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** A session save costs in proportion to what changed, not to how much text is open: text is collected only for the
  * buffers whose text the session keeps, and only when it differs from what the last save already stored.
  */
class SessionSaveWorkSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** A leaf that counts how often anyone asks it for its characters. */
  final private class CountingLeaf(text: String) extends Leaf(text):
    private val collections = new AtomicInteger(0)

    override def collect(): String =
      collections.incrementAndGet()
      text

    def collected: Int = collections.get

  private def cleanBuffer(leaf: CountingLeaf, id: BufferId = BufferId(7)): Buffer =
    Buffer(id, Document(leaf, filePath = Some(Path.of("/notes/clean.txt"))))

  private def untitledBuffer(id: BufferId, leaf: CountingLeaf): Buffer =
    Buffer(id, Document(leaf))

  private def newManager(root: Path): SessionManager =
    SessionManager(
      root,
      AppThemeManager.create,
      NoOpLogger.impl[IO],
      SessionManager.SessionPolicy(),
      clock = SettledClock.aMinuteAhead
    )

  "SessionBuffer.fromBuffer" should "describe a clean buffer without collecting its text" in {
    val leaf    = new CountingLeaf("text that stays on disk")
    val session = SessionBuffer.fromBuffer(cleanBuffer(leaf), persistUnsaved = false)

    session.unsavedContent shouldBe None
    leaf.collected shouldBe 0
  }

  it should "keep a rich text document the version stamp vouches for without collecting the text" in {
    val leaf     = new CountingLeaf("hello")
    val document = RichTextDocument.fromPlainText("hello")
    val buffer =
      cleanBuffer(leaf).copy(richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L))
    val session = SessionBuffer.fromBuffer(buffer, persistUnsaved = false)

    session.richTextDocument shouldBe Some(document)
    leaf.collected shouldBe 0
  }

  it should "compare an unstamped rich text document with the text and drop it when they differ" in {
    val leaf     = new CountingLeaf("edited")
    val document = RichTextDocument.fromPlainText("original")
    val buffer =
      cleanBuffer(leaf).copy(richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 9L))
    val session = SessionBuffer.fromBuffer(buffer, persistUnsaved = false)

    session.richTextDocument shouldBe None
    leaf.collected shouldBe 1
  }

  it should "collect the text of a buffer that has unsaved changes exactly once" in {
    val leaf    = new CountingLeaf("draft")
    val clean   = cleanBuffer(leaf)
    val buffer  = clean.copy(document = clean.document.copy(isDirty = true))
    val session = SessionBuffer.fromBuffer(buffer, persistUnsaved = false)

    session.unsavedContent shouldBe Some("draft")
    leaf.collected shouldBe 1
  }

  "A session save" should "not collect, encode or hash an unchanged buffer a second time" in {
    val manager = newManager(TestTemp.directory("session-save-work"))
    val leaf    = new CountingLeaf("unsaved draft")
    val id      = AppState.initial.persisted.bufferOrder.head
    val clean   = new CountingLeaf("clean")
    val state   = withBuffers(untitledBuffer(id, leaf), cleanBuffer(clean, BufferId(8)))

    manager.saveSession(state).unsafeRunSync()
    val afterFirst = leaf.collected
    manager.saveSession(state).unsafeRunSync()
    manager.saveSession(state).unsafeRunSync()

    afterFirst shouldBe 1
    leaf.collected shouldBe afterFirst
    clean.collected shouldBe 0
  }

  it should "store the new text once the buffer changes, and prune the old content file" in {
    val root    = TestTemp.directory("session-save-work")
    val manager = newManager(root)
    val id      = AppState.initial.persisted.bufferOrder.head
    val first   = new CountingLeaf("first draft")
    val second  = new CountingLeaf("second draft")

    manager.saveSession(withBuffers(untitledBuffer(id, first))).unsafeRunSync()
    manager.saveSession(withBuffers(untitledBuffer(id, second))).unsafeRunSync()

    second.collected shouldBe 1
    contentFiles(root) should have size 1
    restoredTexts(manager) shouldBe List("second draft")
  }

  it should "write a content file that has gone missing again, even for an unchanged buffer" in {
    val root    = TestTemp.directory("session-save-work")
    val manager = newManager(root)
    val id      = AppState.initial.persisted.bufferOrder.head
    val state   = withBuffers(untitledBuffer(id, new CountingLeaf("precious")))

    manager.saveSession(state).unsafeRunSync()
    contentFiles(root).foreach(Files.delete)
    manager.saveSession(state).unsafeRunSync()

    contentFiles(root) should have size 1
    restoredTexts(manager) shouldBe List("precious")
  }

  it should "write nothing when the session is exactly what the last save stored" in {
    val root    = TestTemp.directory("session-save-work")
    val manager = newManager(root)
    val id      = AppState.initial.persisted.bufferOrder.head
    val state   = withBuffers(untitledBuffer(id, new CountingLeaf("draft")))
    val files   = List(root.resolve("session-index.json"), root.resolve("sessions").resolve("session.json"))
    def stamps  = files.map(path => FileStamp.read(path))

    manager.saveSession(state).unsafeRunSync()
    val afterFirst = stamps
    manager.saveSession(state).unsafeRunSync()
    manager.saveSession(state).unsafeRunSync()

    afterFirst.flatten should have size 2
    stamps shouldBe afterFirst
  }

  it should "write the session again when its file was deleted behind the manager's back" in {
    val root    = TestTemp.directory("session-save-work")
    val manager = newManager(root)
    val id      = AppState.initial.persisted.bufferOrder.head
    val state   = withBuffers(untitledBuffer(id, new CountingLeaf("draft")))
    val session = root.resolve("sessions").resolve("session.json")

    manager.saveSession(state).unsafeRunSync()
    Files.delete(session)
    manager.saveSession(state).unsafeRunSync()

    Files.exists(session) shouldBe true
  }

  it should "write the session again when only a non-text part of the state changed" in {
    val root    = TestTemp.directory("session-save-work")
    val manager = newManager(root)
    val id      = AppState.initial.persisted.bufferOrder.head
    val state   = withBuffers(untitledBuffer(id, new CountingLeaf("draft")))
    val session = root.resolve("sessions").resolve("session.json")

    manager.saveSession(state).unsafeRunSync()
    val before  = Files.readString(session)
    val recent  = Path.of("/notes/new.txt")
    val renamed = state.copy(persisted = state.persisted.copy(recentFiles = List(recent)))
    manager.saveSession(renamed).unsafeRunSync()

    Files.readString(session) should not be before
    manager.loadSession().unsafeRunSync().map(_.persisted.recentFiles) shouldBe Some(List(recent))
  }

  private def withBuffers(buffers: Buffer*): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = buffers.map(buffer => buffer.id -> buffer).toMap,
        bufferOrder = buffers.map(_.id).toList
      )
    )

  private def contentFiles(root: Path): List[Path] =
    val directory = root.resolve("sessions").resolve("session.content")
    if Files.isDirectory(directory) then
      val listing = Files.list(directory)
      try listing.iterator().asScala.toList
      finally listing.close()
    else Nil

  private def restoredTexts(manager: SessionManager): List[String] =
    manager
      .loadSession()
      .unsafeRunSync()
      .toList
      .flatMap(_.persisted.buffers.values.filter(_.document.filePath.isEmpty).map(_.document.content.collect()))
