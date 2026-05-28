package com.serenity.session

import cats.effect.IO
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.charset.StandardCharsets
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

/**
 * Manages session persistence - saving and loading workspace state
 */
class SessionManager(
    sessionFile: Path,
    themeManager: AppThemeManager,
    logger: Logger[IO]
):

  /**
   * Save the current app state as a session
   */
  def saveSession(appState: AppState): IO[Unit] =
    logger.debug(s"[SESSION] Saving session to ${sessionFile}") >>
    IO {
      val sessionState = SessionState.fromAppState(appState)
      val jsonString = sessionState.asJson.spaces2
      jsonString
    }.flatMap { jsonString =>
      IO.blocking {
        // Ensure parent directory exists
        Files.createDirectories(sessionFile.getParent)
        Files.write(
          sessionFile, 
          jsonString.getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE, 
          StandardOpenOption.TRUNCATE_EXISTING
        )
      } >> logger.info(s"[SESSION] Session saved successfully")
    }.void

  /**
   * Load a previously saved session, if it exists
   */
  def loadSession()(using com.serenity.rope.Balance): IO[Option[AppState]] =
    if !Files.exists(sessionFile) then
      logger.debug(s"[SESSION] No session file found at ${sessionFile}").as(None)
    else
      loadSessionFromFile().handleErrorWith { error =>
        logger.error(error)(s"[SESSION] Failed to load session from ${sessionFile}").as(None)
      }

  private def loadSessionFromFile()(using com.serenity.rope.Balance): IO[Option[AppState]] =
    for
      _ <- logger.debug(s"[SESSION] Loading session from ${sessionFile}")
      jsonString <- IO.blocking {
        new String(Files.readAllBytes(sessionFile), StandardCharsets.UTF_8)
      }
      sessionState <- IO.fromEither(decode[SessionState](jsonString))
      theme <- themeManager.initializeWithTheme(sessionState.themeName)
        .handleErrorWith(_ => 
          logger.warn(s"[SESSION] Theme '${sessionState.themeName}' not found, using default") >>
          themeManager.initializeWithTheme("dark")
        )
      appState = SessionState.toAppState(sessionState, theme)
      _ <- logger.info(s"[SESSION] Session loaded successfully with ${sessionState.buffers.size} buffers")
    yield Some(appState)

  /**
   * Check if a session file exists
   */
  def sessionExists: IO[Boolean] =
    IO.blocking(Files.exists(sessionFile))

  /**
   * Delete the current session file
   */
  def clearSession(): IO[Unit] =
    IO.blocking {
      IO.whenA(Files.exists(sessionFile))(IO(Files.delete(sessionFile)))
    }.flatten.handleErrorWith { error =>
      logger.error(error)(s"[SESSION] Failed to delete session file ${sessionFile}")
    } >> logger.info(s"[SESSION] Session file cleared")

object SessionManager:

  /**
   * Create a SessionManager with default session file location
   */
  def create(themeManager: AppThemeManager, logger: Logger[IO]): SessionManager =
    val sessionFile = defaultSessionFile()
    new SessionManager(sessionFile, themeManager, logger)

  /**
   * Create a SessionManager with custom session file location
   */
  def create(sessionFile: Path, themeManager: AppThemeManager, logger: Logger[IO]): SessionManager =
    new SessionManager(sessionFile, themeManager, logger)

  /**
   * Get the default session file location
   */
  def defaultSessionFile(): Path =
    val userHome = System.getProperty("user.home")
    Paths.get(userHome, ".serenity", "session.json")

  /**
   * Session persistence policy configuration
   */
  case class SessionPolicy(
      saveOnFileChange: Boolean = true,
      saveOnAppClose: Boolean = true,
      saveInterval: Option[scala.concurrent.duration.FiniteDuration] = None,
      persistUnsavedBuffers: Boolean = false,
      maxSessionHistory: Int = 5
  )

/**
 * Higher-level session operations
 */
trait SessionOperations:
  def saveSession(appState: AppState): IO[Unit]
  def loadSession()(using com.serenity.rope.Balance): IO[Option[AppState]]
  def sessionExists: IO[Boolean]
  def clearSession(): IO[Unit]

/**
 * Session persistence integration for StateManager
 */
class SessionPersistence(
    sessionManager: SessionManager,
    policy: SessionManager.SessionPolicy,
    logger: Logger[IO]
):

  /**
   * Trigger session save based on policy
   */
  def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
    val shouldSave = trigger match
      case SessionSaveTrigger.FileChange => policy.saveOnFileChange
      case SessionSaveTrigger.AppClose => policy.saveOnAppClose
      case SessionSaveTrigger.Manual => true
      case SessionSaveTrigger.Interval => policy.saveInterval.isDefined

    if shouldSave then
      sessionManager.saveSession(appState)
    else
      IO.unit

  /**
   * Auto-save session when buffers change
   */
  def onBufferChange(appState: AppState): IO[Unit] =
    maybeSaveSession(appState, SessionSaveTrigger.FileChange)

  /**
   * Save session on application close
   */
  def onAppClose(appState: AppState): IO[Unit] =
    maybeSaveSession(appState, SessionSaveTrigger.AppClose)

enum SessionSaveTrigger:
  case FileChange
  case AppClose
  case Manual
  case Interval