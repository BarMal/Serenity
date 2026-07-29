package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.util.UUID

import scala.jdk.CollectionConverters.*
import scala.util.Try

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

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

  private val sessionsRootAbsolute = sessionsDirectory.toAbsolutePath.normalize

  /** Save the current app state to the current session, creating one if needed.
    */
  def saveSession(appState: AppState): IO[Unit] =
    saveSession(appState, policy.persistUnsavedBuffers)

  /** Save the current app state to the current session with an explicit unsaved-content persistence mode.
    */
  def saveSession(appState: AppState, persistUnsavedBuffers: Boolean): IO[Unit] =
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
      canonicalMetadata = metadata.copy(sessionFileName = canonicalSessionFileName(sessionId))
      _ <- writeSessionFile(canonicalMetadata.sessionFileName, appState, persistUnsavedBuffers)
      updatedMetadata = canonicalMetadata.copy(updatedAtEpochMillis = now)
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
        sessionFileName = canonicalSessionFileName(sessionId),
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
    readIndex().flatMap { index =>
      index.currentSessionId
        .flatMap(sessionId => index.sessions.find(_.id == sessionId))
        .flatMap(metadata => safeSessionPath(metadata.sessionFileName)) match
        case Some(path) => IO.blocking(Files.exists(path))
        case None       => IO.pure(false)
    }

  /** Read the current session's saved theme name without restoring the full session.
    */
  def currentSessionThemeName: IO[Option[String]] =
    readIndex().flatMap { index =>
      index.currentSessionId
        .flatMap(sessionId => index.sessions.find(_.id == sessionId))
        .flatMap(metadata => safeSessionPath(metadata.sessionFileName)) match
        case Some(path) =>
          IO.blocking(Files.exists(path)).flatMap {
            case false => IO.pure(None)
            case true =>
              readUtf8(path)
                .flatMap(jsonString => IO.fromEither(_root_.io.circe.parser.decode[SessionState](jsonString)))
                .map(sessionState => Some(sessionState.themeName))
                .handleErrorWith(error =>
                  logger.warn(error)(s"[SESSION] Failed to read current session theme from $path").as(None)
                )
          }
        case None => IO.pure(None)
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
    safeSessionPath(sessionFileName) match
      case None => IO.pure(None)
      case Some(sessionFile) =>
        IO.blocking(Files.exists(sessionFile))
          .flatMap {
            case false => logger.debug(s"[SESSION] No session file found at $sessionFile").as(None)
            case true =>
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
                baselineTheme <- sessionState.uiPresetEditSession.traverse { session =>
                  themeManager.initializeWithTheme(session.baselineThemeName).handleErrorWith(_ => IO.pure(theme))
                }
                appState <- SessionState.toAppStateIO(sessionState, theme)
                restored = baselineTheme.fold(appState)(baseline =>
                  appState.copy(
                    uiPresetEditSession = sessionState.uiPresetEditSession.map(
                      SessionUiPresetEditSession.toSession(_, baseline)
                    )
                  )
                )
                _ <- logger.info(s"[SESSION] Session loaded successfully with ${sessionState.buffers.size} buffers")
              yield Some(restored)
          }
          .handleErrorWith(recoverFailedSessionFile(sessionFile, _))

  private def recoverFailedSessionFile(sessionFile: Path, error: Throwable): IO[Option[AppState]] =
    quarantineSessionFile(sessionFile).attempt.flatMap {
      case Right(Some(quarantineFile)) =>
        logger.error(error)(s"[SESSION] Failed to load session file at $sessionFile; copied to $quarantineFile") >>
          IO.pure(None)
      case Right(None) =>
        logger.error(error)(s"[SESSION] Failed to load session file at $sessionFile; no file was available to copy") >>
          IO.pure(None)
      case Left(quarantineError) =>
        logger.error(quarantineError)(s"[SESSION] Failed to copy corrupt session file at $sessionFile") >>
          logger.error(error)(s"[SESSION] Failed to load session file at $sessionFile") >>
          IO.pure(None)
    }

  private def quarantineSessionFile(sessionFile: Path): IO[Option[Path]] =
    IO.blocking(Files.exists(sessionFile)).flatMap {
      case false => IO.pure(None)
      case true =>
        currentTimeMillis().flatMap { now =>
          val quarantineFile = sessionFile.resolveSibling(s"${sessionFile.getFileName}.corrupt-$now")
          IO.blocking(Files.copy(sessionFile, quarantineFile, StandardCopyOption.REPLACE_EXISTING))
            .as(Some(quarantineFile))
        }
    }

  private def readIndex(): IO[SessionIndex] =
    IO.blocking(Files.exists(indexFile)).flatMap {
      case false => IO.pure(SessionIndex.empty)
      case true =>
        readUtf8(indexFile)
          .flatMap(jsonString => IO.fromEither(_root_.io.circe.parser.decode[SessionIndex](jsonString)))
          .flatMap(sanitizeIndex)
          .handleErrorWith(error => recoverCorruptIndex(error))
    }

  private def sanitizeIndex(index: SessionIndex): IO[SessionIndex] =
    index.sessions
      .traverse { metadata =>
        safeSessionPath(metadata.sessionFileName) match
          case Some(_) => IO.pure(Some(metadata))
          case None =>
            logger.error(
              s"[SESSION] Ignoring unsafe session path '${metadata.sessionFileName}' for ${metadata.id.value}"
            ) >>
              IO.pure(None)
      }
      .map { sessions =>
        val safeSessions = sessions.flatten
        index.copy(
          sessions = safeSessions,
          currentSessionId = index.currentSessionId.filter(id => safeSessions.exists(_.id == id))
        )
      }

  private def recoverCorruptIndex(error: Throwable): IO[SessionIndex] =
    quarantineIndexFile.attempt.flatMap {
      case Right(Some(quarantineFile)) =>
        logger.error(error)(s"[SESSION] Failed to read session index at $indexFile; copied to $quarantineFile") >>
          recoverIndexFromSessionFiles
      case Right(None) =>
        logger.error(error)(s"[SESSION] Failed to read session index at $indexFile; no file was available to copy") >>
          recoverIndexFromSessionFiles
      case Left(quarantineError) =>
        logger.error(quarantineError)(s"[SESSION] Failed to copy corrupt session index at $indexFile") >>
          logger.error(error)(s"[SESSION] Failed to read session index at $indexFile") >>
          recoverIndexFromSessionFiles
    }

  private def quarantineIndexFile: IO[Option[Path]] =
    IO.blocking(Files.exists(indexFile)).flatMap {
      case false => IO.pure(None)
      case true =>
        currentTimeMillis().flatMap { now =>
          val quarantineFile = indexFile.resolveSibling(s"${indexFile.getFileName}.corrupt-$now")
          IO.blocking(Files.copy(indexFile, quarantineFile, StandardCopyOption.REPLACE_EXISTING))
            .as(Some(quarantineFile))
        }
    }

  private def recoverIndexFromSessionFiles: IO[SessionIndex] =
    IO.blocking {
      val recovered: List[SessionMetadata] =
        if !Files.isDirectory(sessionsDirectory) then Nil
        else
          val stream = Files.list(sessionsDirectory)
          try
            stream
              .iterator()
              .asScala
              .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".json"))
              .flatMap(path => recoveredMetadata(path))
              .toList
          finally stream.close()
      recovered
    }.map { (metadata: List[SessionMetadata]) =>
      SessionIndex(
        sessions = metadata.sortBy(_.sessionFileName),
        currentSessionId = Option.when(metadata.exists(_.id == defaultSessionId))(defaultSessionId)
      )
    }

  private def recoveredMetadata(path: Path): Option[SessionMetadata] =
    val fileName = path.getFileName.toString
    val id = if fileName == defaultSessionFileName then defaultSessionId else SessionId(fileName.stripSuffix(".json"))
    safeSessionPath(fileName).flatMap { _ =>
      Option(Try(Files.getLastModifiedTime(path).toMillis).getOrElse(0L)).map { updatedAt =>
        SessionMetadata(id, id.value, fileName, updatedAt, updatedAt)
      }
    }

  private def writeIndex(index: SessionIndex): IO[Unit] =
    writeUtf8(indexFile, _root_.io.circe.syntax.EncoderOps(index).asJson.spaces2)

  private def writeSessionFile(
    sessionFileName: String,
    appState: AppState,
    persistUnsavedBuffers: Boolean = policy.persistUnsavedBuffers
  ): IO[Unit] =
    val sessionState = SessionState.fromAppState(appState, persistUnsaved = persistUnsavedBuffers)
    safeSessionPath(sessionFileName) match
      case None => IO.raiseError(new IllegalArgumentException(s"Unsafe session path: $sessionFileName"))
      case Some(sessionFile) =>
        writeUtf8(sessionFile, _root_.io.circe.syntax.EncoderOps(sessionState).asJson.spaces2)

  private def deleteSessionFile(sessionFileName: String): IO[Unit] =
    safeSessionPath(sessionFileName) match
      case None => IO.unit
      case Some(sessionFile) =>
        IO.blocking {
          if Files.exists(sessionFile) then Files.delete(sessionFile)
        }.handleErrorWith(error => logger.error(error)(s"[SESSION] Failed to delete session file $sessionFile"))

  private def writeUtf8(path: Path, value: String): IO[Unit] =
    AtomicFileWriter.writeString(path, value)

  private def readUtf8(path: Path): IO[String] =
    IO.blocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))

  private def canonicalSessionFileName(sessionId: SessionId): String =
    if sessionId == defaultSessionId then defaultSessionFileName else s"${sessionId.value}.json"

  private def safeSessionPath(sessionFileName: String): Option[Path] =
    val portableName = sessionFileName.replace('\\', '/')
    Try {
      val path            = Paths.get(portableName)
      val resolved        = sessionsRootAbsolute.resolve(portableName).normalize
      val windowsAbsolute = portableName.matches("^[A-Za-z]:/.*") || portableName.startsWith("//")
      val relative        = sessionsRootAbsolute.relativize(resolved)
      val hasSymlink = (0 until relative.getNameCount).exists { index =>
        Files.isSymbolicLink(sessionsRootAbsolute.resolve(relative.subpath(0, index + 1)))
      }
      if !windowsAbsolute && !path.isAbsolute && resolved.startsWith(
            sessionsRootAbsolute
          ) && resolved != sessionsRootAbsolute && !hasSymlink
      then Some(resolved)
      else None
    }.toOption.flatten

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
      maxSessionHistory: Int = 5,
      maxUndoDepth: Int = 1000
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
