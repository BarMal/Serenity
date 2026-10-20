package com.serenity

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import _root_.io.circe.{Json, JsonObject}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.session.{SessionManager, SessionState, UnreadableSession}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Release readiness, "no downgrade": an older Serenity run over session files a newer one wrote must start, must not
  * lose the user's unsaved text, and must not overwrite what it could not read. The config file's half is
  * `ConfigVersioningSpec`.
  */
class SessionDowngradeSpec extends AnyFlatSpec with Matchers with OptionValues with StateManagerTestSupport:

  private val unsaved       = "text typed into a newer build"
  private val newerSchema   = SessionState.CurrentSchemaVersion.value + 1
  private val unknownTag    = Json.fromString("tag-from-the-future")
  private val futureIndexed = "indexed-by-a-newer-build"

  private def managerAt(root: Path, policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy.interactive) =
    SessionManager.create(root, AppThemeManager.create, testLogger("SessionDowngradeSpec"), policy)

  private def sessionsDirectory(root: Path): Path = root.resolve("sessions")
  private def sessionFile(root: Path): Path       = sessionsDirectory(root).resolve("session.json")
  private def indexFile(root: Path): Path         = root.resolve("session-index.json")

  private def entries(root: Path): List[String] =
    val stream = Files.list(sessionsDirectory(root))
    try stream.iterator.asScala.map(_.getFileName.toString).toList.sorted
    finally stream.close()

  /** The bytes of every `<fileName>.newer-*` backup: the files themselves, not their `.content` or `.recovered`
    * folders.
    */
  private def newerBackups(root: Path, fileName: String): List[List[Byte]] =
    entries(root)
      .filter(_.startsWith(s"$fileName.newer-"))
      .filterNot(name => name.endsWith(".content") || name.endsWith(".recovered"))
      .map(name => Files.readAllBytes(sessionsDirectory(root).resolve(name)).toList)

  private def bytes(path: Path): List[Byte] = Files.readAllBytes(path).toList

  private def dirtyState(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    val dirty    = buffer.copy(document = buffer.document.copy(isDirty = true))
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> dirty)))

  private def texts(state: AppState): List[String] =
    state.persisted.buffers.values.map(_.document.content.collect()).toList

  private def edited(path: Path)(change: JsonObject => JsonObject): IO[Unit] =
    IO.blocking {
      val json = _root_.io.circe.parser.parse(Files.readString(path)).toOption.value
      Files.writeString(path, json.mapObject(change).spaces2)
      ()
    }

  private def withField(name: String)(change: Json => Json): JsonObject => JsonObject =
    obj => obj(name).fold(obj)(value => obj.add(name, change(value)))

  private def everyStringIs(replacement: Json)(json: Json): Json =
    json.fold(
      json,
      _ => json,
      _ => json,
      _ => replacement,
      array => Json.fromValues(array.map(everyStringIs(replacement))),
      obj => Json.fromJsonObject(obj.mapValues(everyStringIs(replacement)))
    )

  private val toNewerSchema: JsonObject => JsonObject = _.add("schemaVersion", Json.fromInt(newerSchema))

  /** A real saved session holding `unsaved` as unsaved text, then rewritten by `damage`. */
  private def newerSession(damage: JsonObject => JsonObject): IO[Path] =
    for
      root <- IO.blocking(TestTemp.directory("session-downgrade"))
      _    <- managerAt(root).saveSession(dirtyState(unsaved))
      _    <- edited(sessionFile(root))(damage)
    yield root

  private def launch(root: Path): IO[AppState] =
    StateManager(
      testLogger("SessionDowngradeSpec"),
      sessionRootOverride = Some(root),
      dictionaryCache = SharedDictionary.default
    ).flatMap { stateManager =>
      AppStartup
        .initializeState(stateManager, stateManager.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
        .timeout(30.seconds)
    }

  private def promptTitles(state: AppState): List[String] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt.title }

  private def recoveredTexts(aside: UnreadableSession): List[String] =
    aside.recoveredTexts.map(Files.readString)

  /** What every downgrade case has to meet: the session loads with the text, or the file is kept byte for byte and the
    * text is exported beside it.
    */
  private def textSurvives(root: Path): IO[Unit] =
    val manager = managerAt(root)
    for
      original <- IO.blocking(bytes(sessionFile(root)))
      aside    <- manager.setAsideUnreadableCurrentSession()
      loaded   <- manager.loadSession()
    yield aside match
      case None =>
        loaded.map(texts).value should contain(unsaved)
      case Some(kept) =>
        bytes(kept.backup) shouldBe original
        Files.exists(sessionFile(root)) shouldBe false
        recoveredTexts(kept) should contain(unsaved)

  // -- (a) a session file of a schema this build does not know ------------------------------------------------------

  "A session of a newer schema" should "be set aside byte for byte with its content, and startup should say so" in {
    val program = for
      root     <- newerSession(toNewerSchema)
      original <- IO.blocking(bytes(sessionFile(root)))
      state    <- launch(root)
    yield
      promptTitles(state) should contain("Session not restored")
      newerBackups(root, "session.json") shouldBe List(original)
      entries(root).exists(name => name.startsWith("session.json.newer-") && name.endsWith(".content")) shouldBe true

    program.unsafeRunSync()
  }

  it should "not be overwritten by a save from the older build that starts without setting it aside" in {
    val program = for
      root     <- newerSession(toNewerSchema)
      original <- IO.blocking(bytes(sessionFile(root)))
      _        <- managerAt(root).saveSession(dirtyState("typed after the downgrade"))
    yield newerBackups(root, "session.json") shouldBe List(original)

    program.unsafeRunSync()
  }

  it should "not be overwritten by a save when it appears after the older build has already saved" in {
    val program = for
      root <- IO.blocking(TestTemp.directory("session-downgrade-late"))
      manager = managerAt(root)
      _        <- manager.saveSession(dirtyState("first save"))
      _        <- manager.saveSession(dirtyState("second save"))
      _        <- edited(sessionFile(root))(toNewerSchema)
      original <- IO.blocking(bytes(sessionFile(root)))
      _        <- manager.saveSession(dirtyState("third save"))
    yield newerBackups(root, "session.json") shouldBe List(original)

    program.unsafeRunSync()
  }

  // -- (b) unknown extra fields -------------------------------------------------------------------------------------

  "A session with fields this build does not know" should "load with its text, untouched, and startup should not prompt" in {
    val future = Json.obj("nested" -> Json.arr(Json.fromInt(1), Json.Null))
    val program = for
      root <- newerSession(
        _.add("futureTopLevel", future)
          .pipe(withField("config")(_.mapObject(_.add("futureConfigKey", future))))
          .pipe(withField("layout")(_.mapObject(_.add("futureLayoutKey", future))))
          .pipe(withField("buffers")(_.mapArray(_.map(_.mapObject(_.add("futureBufferField", future))))))
      )
      before <- IO.blocking(bytes(sessionFile(root)))
      loaded <- managerAt(root).loadSession()
      aside  <- managerAt(root).setAsideUnreadableCurrentSession()
      state  <- launch(root)
    yield
      aside shouldBe None
      texts(loaded.value) should contain(unsaved)
      promptTitles(state) should not contain "Session not restored"
      bytes(sessionFile(root)) shouldBe before

    program.unsafeRunSync()
  }

  it should "stay loadable after the older build saves over it" in {
    val program = for
      root <- newerSession(_.add("futureTopLevel", Json.fromString("kept or dropped, never fatal")))
      manager = managerAt(root)
      loaded   <- manager.loadSession()
      _        <- manager.saveSession(loaded.value)
      reopened <- managerAt(root).loadSession()
    yield texts(reopened.value) should contain(unsaved)

    program.unsafeRunSync()
  }

  // -- (c) unknown enum or tag values in known fields ---------------------------------------------------------------

  "A session with a tag this build does not know" should "keep the text when config values are unknown" in {
    val program = newerSession(withField("config")(everyStringIs(unknownTag))).flatMap(textSurvives)

    program.unsafeRunSync()
  }

  it should "keep the text when focus and layout tags are unknown" in {
    val program = newerSession(
      withField("layout")(everyStringIs(unknownTag)).andThen(withField("focus")(everyStringIs(unknownTag)))
    ).flatMap(textSurvives)

    program.unsafeRunSync()
  }

  it should "keep the text when a rich-text paragraph role or mark is unknown" in {
    val run = Json.obj(
      "text"  -> Json.fromString(unsaved),
      "style" -> Json.obj("marks" -> Json.arr(unknownTag))
    )
    val richText = Json.obj(
      "paragraphs" -> Json.arr(Json.obj("runs" -> Json.arr(run), "role" -> Json.obj("type" -> unknownTag)))
    )
    val program = newerSession(
      withField("buffers")(_.mapArray(_.map(_.mapObject(_.add("richTextDocument", richText)))))
    ).flatMap(textSurvives)

    program.unsafeRunSync()
  }

  it should "let startup finish" in {
    val program = newerSession(withField("config")(everyStringIs(unknownTag))).flatMap(launch)

    program.unsafeRunSync().persisted.buffers should not be null
  }

  // -- (d) an index naming a session of a newer schema --------------------------------------------------------------

  /** A default session and a named one the newer build saved, the index also carrying a field this build lacks. */
  private def indexedNewerNamedSession(makeCurrent: Boolean): IO[(Path, String)] =
    for
      root <- IO.blocking(TestTemp.directory("session-downgrade-index"))
      manager = managerAt(root)
      _  <- manager.saveSession(dirtyState("the default session"))
      id <- manager.saveSessionAs(futureIndexed, dirtyState(unsaved))
      file = s"${id.value}.json"
      _ <- edited(sessionsDirectory(root).resolve(file))(toNewerSchema)
      _ <- edited(indexFile(root))(
        withField("currentSessionId")(old => if makeCurrent then Json.fromString(id.value) else old)
          .andThen(_.add("indexRevisionFromNewerBuild", Json.fromInt(7)))
          .andThen(withField("sessions")(_.mapArray(_.map(_.mapObject(_.add("tags", Json.arr(unknownTag)))))))
      )
    yield (root, file)

  "An index naming a newer-schema session" should "have it set aside, intact, when it is the current one" in {
    val program = for
      (root, file) <- indexedNewerNamedSession(makeCurrent = true)
      original     <- IO.blocking(bytes(sessionsDirectory(root).resolve(file)))
      state        <- launch(root)
    yield
      promptTitles(state) should contain("Session not restored")
      newerBackups(root, file) shouldBe List(original)

    program.unsafeRunSync()
  }

  it should "keep a non-current one intact when it is opened" in {
    val program = for
      (root, file) <- indexedNewerNamedSession(makeCurrent = false)
      original     <- IO.blocking(bytes(sessionsDirectory(root).resolve(file)))
      manager = managerAt(root)
      sessions <- manager.listSessions()
      opened   <- manager.loadSession(sessions.find(_.displayName == futureIndexed).value.id)
    yield
      opened shouldBe None
      newerBackups(root, file) shouldBe List(original)

    program.unsafeRunSync()
  }

  it should "not have one it cannot read deleted by the older build's history limit" in {
    val program = for
      (root, file) <- indexedNewerNamedSession(makeCurrent = false)
      original     <- IO.blocking(bytes(sessionsDirectory(root).resolve(file)))
      manager = managerAt(root, SessionManager.SessionPolicy(maxSessionHistory = 1))
      _ <- manager.saveSessionAs("named by the older build", dirtyState("typed after the downgrade"))
    yield
      val inPlace = Option(sessionsDirectory(root).resolve(file)).filter(Files.exists(_)).map(bytes)
      (inPlace.toList ++ newerBackups(root, file)) should contain(original)

    program.unsafeRunSync()
  }

  it should "start the older build even though the index carries fields it does not know" in {
    val program = for
      (root, _) <- indexedNewerNamedSession(makeCurrent = false)
      sessions  <- managerAt(root).listSessions()
      state     <- launch(root)
    yield
      sessions.map(_.displayName) should contain(futureIndexed)
      state.persisted.buffers should not be null

    program.unsafeRunSync()
  }

end SessionDowngradeSpec
