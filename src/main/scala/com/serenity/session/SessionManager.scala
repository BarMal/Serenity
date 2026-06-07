package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.util.UUID

import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager

/** Manages session persistence - saving and loading workspace state.
  */
class SessionManager(
    sessionRoot: Path,
    themeManager: AppThemeManager,
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
):

  private val indexFile: Path         = sessionRoot.resolve("session-index.json")
  private val sessionsDirectory: Path = sessionRoot.resolve("sessions")
  private val defaultSessionId        = SessionId("current")
  private val defaultSessionName      = "Last Session"
  private val defaultSessionFileName  = "session.json"

  /** Save the current app state to the current session, creating one if needed.
    */
  def saveSession(appState: AppState): IO[Unit] =
    for
      now   <- currentTimeMillis()
      index <- readIndex()
      sessionId = index.currentSessionId.getOrElse(defaultSessionId)
      metadata = index.sessions
        .find(_.id == sessionId)
        .getOrElse(
          SessionMetadata(
            id = defaultSessionId,
            displayName = defaultSessionName,
            sessionFileName = defaultSessionFileName,
            createdAtEpochMillis = now,
            updatedAtEpochMillis = now
          )
        )
      _ <- writeSessionFile(metadata.sessionFileName, appState)
      updatedMetadata = metadata.copy(updatedAtEpochMillis = now)
      updatedIndex    = upsertSession(index, updatedMetadata).copy(currentSessionId = Some(updatedMetadata.id))
      _ <- writeIndex(updatedIndex)
      _ <- logger.info(s"[SESSION] Session saved successfully (${updatedMetadata.displayName})")
    yield ()

  /** Save the current app state as a named session and make it current.
    */
  def saveSessionAs(displayName: String, appState: AppState): IO[SessionId] =
    for
      now <- currentTimeMillis()
      sessionId = SessionId(UUID.randomUUID().toString)
      metadata = SessionMetadata(
        id = sessionId,
        displayName = displayName,
        sessionFileName = s"${sessionId.value}.json",
        createdAtEpochMillis = now,
        updatedAtEpochMillis = now,
        lastOpenedAtEpochMillis = Some(now)
      )
      index <- readIndex()
      _     <- writeSessionFile(metadata.sessionFileName, appState)
      withNew = upsertSession(index, metadata)
      pruned <- pruneHistory(withNew)
      updatedIndex = pruned.copy(currentSessionId = Some(sessionId))
      _ <- writeIndex(updatedIndex)
      _ <- logger.info(s"[SESSION] Named session saved (${metadata.displayName})")
    yield sessionId

  /** Load the current session, if one exists.
    */
  def loadSession()(using com.serenity.rope.Balance): IO[Option[AppState]] =
    readIndex().flatMap {
      _.currentSessionId match
        case Some(sessionId) => loadSession(sessionId)
        case None =>
          logger.debug("[SESSION] No current session selected").as(None)
    }

  /** Load a session by id and mark it current.
    */
  def loadSession(sessionId: SessionId)(using com.serenity.rope.Balance): IO[Option[AppState]] =
    readIndex().flatMap { index =>
      index.sessions.find(_.id == sessionId) match
        case Some(metadata) =>
          loadSessionFile(metadata.sessionFileName).flatTap {
            case Some(_) =>
              currentTimeMillis().flatMap { now =>
                val touched = metadata.copy(
                  updatedAtEpochMillis = math.max(metadata.updatedAtEpochMillis, now),
                  lastOpenedAtEpochMillis = Some(now)
                )
                writeIndex(upsertSession(index, touched).copy(currentSessionId = Some(sessionId)))
              }
            case None =>
              IO.unit
          }
        case None =>
          logger.debug(s"[SESSION] No session metadata found for ${sessionId.value}").as(None)
    }

  /** List saved sessions in index order.
    */
  def listSessions(): IO[List[SessionMetadata]] =
    readIndex().map(_.sessions)

  /** Rename a saved session.
    */
  def renameSession(sessionId: SessionId, newDisplayName: String): IO[Unit] =
    for
      now   <- currentTimeMillis()
      index <- readIndex()
      updatedIndex = index.rename(sessionId, newDisplayName, now)
      _ <- writeIndex(updatedIndex)
      _ <- logger.info(s"[SESSION] Session renamed to '$newDisplayName'")
    yield ()

  /** Delete a saved session by id.
    */
  def deleteSession(sessionId: SessionId): IO[Unit] =
    for
      index <- readIndex()
      sessionFileNames = index.sessions.filter(_.id == sessionId).map(_.sessionFileName)
      _ <- sessionFileNames.traverse_(deleteSessionFile)
      _ <- writeIndex(index.remove(sessionId))
      _ <- logger.info(s"[SESSION] Session deleted (${sessionId.value})")
    yield ()

  /** Check if the current session exists.
    */
  def sessionExists: IO[Boolean] =
    readIndex().map { index =>
      index.currentSessionId
        .flatMap(sessionId => index.sessions.find(_.id == sessionId))
        .map(metadata => sessionsDirectory.resolve(metadata.sessionFileName))
        .exists(path => Files.exists(path))
    }

  /** Delete the current session.
    */
  def clearSession(): IO[Unit] =
    readIndex().flatMap {
      _.currentSessionId match
        case Some(sessionId) => deleteSession(sessionId)
        case None            => logger.info("[SESSION] No current session to clear")
    }

  private def loadSessionFile(sessionFileName: String)(using com.serenity.rope.Balance): IO[Option[AppState]] =
    val sessionFile = sessionsDirectory.resolve(sessionFileName)
    if !Files.exists(sessionFile) then logger.debug(s"[SESSION] No session file found at $sessionFile").as(None)
    else
      for
        _            <- logger.debug(s"[SESSION] Loading session from $sessionFile")
        jsonString   <- readUtf8(sessionFile)
        sessionState <- IO.fromEither(_root_.io.circe.parser.decode[SessionState](jsonString))
        theme <- themeManager
          .initializeWithTheme(sessionState.themeName)
          .handleErrorWith(_ =>
            logger.warn(s"[SESSION] Theme '${sessionState.themeName}' not found, using default") >>
              themeManager.initializeWithTheme("dark")
          )
        appState = SessionState.toAppState(sessionState, theme)
        _ <- logger.info(s"[SESSION] Session loaded successfully with ${sessionState.buffers.size} buffers")
      yield Some(appState)

  private def readIndex(): IO[SessionIndex] =
    if !Files.exists(indexFile) then IO.pure(SessionIndex.empty)
    else
      readUtf8(indexFile)
        .flatMap(jsonString => IO.fromEither(_root_.io.circe.parser.decode[SessionIndex](jsonString)))
        .handleErrorWith { error =>
          logger.error(error)(s"[SESSION] Failed to read session index at $indexFile") >>
            IO.pure(SessionIndex.empty)
        }

  private def writeIndex(index: SessionIndex): IO[Unit] =
    writeUtf8(indexFile, _root_.io.circe.syntax.EncoderOps(index).asJson.spaces2)

  private def writeSessionFile(sessionFileName: String, appState: AppState): IO[Unit] =
    val sessionState = SessionState.fromAppState(appState, persistUnsaved = policy.persistUnsavedBuffers)
    writeUtf8(
      sessionsDirectory.resolve(sessionFileName),
      _root_.io.circe.syntax.EncoderOps(sessionState).asJson.spaces2
    )

  private def deleteSessionFile(sessionFileName: String): IO[Unit] =
    val sessionFile = sessionsDirectory.resolve(sessionFileName)
    IO.blocking {
      if Files.exists(sessionFile) then Files.delete(sessionFile)
    }.handleErrorWith(error => logger.error(error)(s"[SESSION] Failed to delete session file $sessionFile"))

  private def writeUtf8(path: Path, value: String): IO[Unit] =
    IO.blocking {
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.write(
        path,
        value.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

  private def readUtf8(path: Path): IO[String] =
    IO.blocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))

  private def upsertSession(index: SessionIndex, metadata: SessionMetadata): SessionIndex =
    val updatedSessions =
      index.sessions.filterNot(_.id == metadata.id) :+ metadata

    index.copy(sessions = updatedSessions)

  private def pruneHistory(index: SessionIndex): IO[SessionIndex] =
    val namedSessions = index.sessions.filterNot(_.id == defaultSessionId)
    val excess        = namedSessions.size - policy.maxSessionHistory
    if excess <= 0 then IO.pure(index)
    else
      val toPrune = namedSessions.sortBy(_.updatedAtEpochMillis).take(excess)
      toPrune.traverse_(s => deleteSessionFile(s.sessionFileName)).map { _ =>
        val pruned = toPrune.map(_.id).toSet
        index.copy(
          sessions = index.sessions.filterNot(s => pruned.contains(s.id)),
          currentSessionId = index.currentSessionId.filterNot(pruned.contains)
        )
      }

  private def currentTimeMillis(): IO[Long] =
    IO.realTime.map(_.toMillis)

object SessionManager:

  /** Create a SessionManager with the default session root directory.
    */
  def create(
    themeManager: AppThemeManager,
    logger: Logger[IO],
    policy: SessionPolicy = SessionPolicy()
  ): SessionManager =
    new SessionManager(defaultSessionRoot(), themeManager, logger, policy)

  /** Create a SessionManager with a custom session root directory.
    */
  def create(
    sessionRoot: Path,
    themeManager: AppThemeManager,
    logger: Logger[IO],
    policy: SessionPolicy
  ): SessionManager =
    new SessionManager(sessionRoot, themeManager, logger, policy)

  /** Get the default session root directory.
    */
  def defaultSessionRoot(): Path =
    val userHome = System.getProperty("user.home")
    Paths.get(userHome, ".serenity")

  /** Session persistence policy configuration.
    */
  case class SessionPolicy(
      saveOnFileChange: Boolean = true,
      saveOnAppClose: Boolean = true,
      saveInterval: Option[scala.concurrent.duration.FiniteDuration] = None,
      persistUnsavedBuffers: Boolean = false,
      maxSessionHistory: Int = 5
  )

/** Higher-level session operations.
  */
trait SessionOperations:
  def saveSession(appState: AppState): IO[Unit]
  def loadSession()(using com.serenity.rope.Balance): IO[Option[AppState]]
  def sessionExists: IO[Boolean]
  def clearSession(): IO[Unit]

/** Session persistence integration for StateManager.
  */
class SessionPersistence(
    sessionManager: SessionManager,
    policy: SessionManager.SessionPolicy,
    logger: Logger[IO]
):

  /** Trigger session save based on policy.
    */
  def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
    val shouldSave = trigger match
      case SessionSaveTrigger.FileChange => policy.saveOnFileChange
      case SessionSaveTrigger.AppClose   => policy.saveOnAppClose
      case SessionSaveTrigger.Manual     => true
      case SessionSaveTrigger.Interval   => policy.saveInterval.isDefined

    if shouldSave then sessionManager.saveSession(appState)
    else IO.unit

  /** Auto-save session when buffers change.
    */
  def onBufferChange(appState: AppState): IO[Unit] =
    maybeSaveSession(appState, SessionSaveTrigger.FileChange)

  /** Save session on application close.
    */
  def onAppClose(appState: AppState): IO[Unit] =
    maybeSaveSession(appState, SessionSaveTrigger.AppClose)

enum SessionSaveTrigger:
  case FileChange
  case AppClose
  case Manual
  case Interval
