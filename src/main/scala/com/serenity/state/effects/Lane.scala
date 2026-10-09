package com.serenity.state.effects

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{BufferId, SurfaceId}

enum LaneKey:
  /** Callers pass canonical paths, so two buffers on one file share a lane. */
  case File(path: Path)
  case Buffer(id: BufferId)

  /** Separate from [[Buffer]] so other switch-latest work on the buffer cannot cancel a pending preview commit. */
  case MarkdownPreview(id: BufferId)
  case Lsp(language: LanguageId)
  case Directory(path: Path)

  /** One explorer panel's listing of `path`: a newer listing supersedes it, another panel's listing of the same
    * directory does not.
    */
  case ExplorerListing(surfaceId: SurfaceId, path: Path)

  /** Docked outline panels' re-parse after an edit burst: a newer edit supersedes the pending one. */
  case OutlineRefresh

  /** The "Go to File" finder's walk of the project: reopening the finder supersedes a walk still running. */
  case ProjectFiles

  /** The project probe a command palette opening starts: reopening the palette supersedes a probe still running. */
  case ProjectProbe

  /** The pause after an edit before the session is saved for crash recovery: a newer edit restarts it. */
  case EditIdleSessionSave

  /** The pause after an edit to `id` before auto-save writes its file: a newer edit restarts it (#1992). */
  case AutoSaveDelay(id: BufferId)

  /** Auto-save's queueing of writes, one after another. */
  case AutoSave

  /** The wait before notices due at `deadlineNanos` are swept away: a lane per deadline, so each runs out on its own.
    */
  case NoticeExpiry(deadlineNanos: Long)

  case Search, Analysis, Theme, Config, Presets, Keybindings, Session, Project, Dialog, Timer

enum LanePolicy:
  /** FIFO per key. */
  case Sequential

  /** Cancel the running job for the key and run the new one; a job still waiting to start is superseded. */
  case SwitchLatest

  /** Ignore the new job if one is running for the key. */
  case DropIfBusy

/** Where an effect runs. A lane is identified by its key *and* policy, so one key used under two policies is two
  * independent lanes with no ordering between them.
  */
enum Lane:
  case Keyed(key: LaneKey, policy: LanePolicy)

  /** No I/O: the dispatcher interprets it synchronously, so it is never submitted to [[EffectLanes]]. */
  case Inline

  /** Lets Sequential work finish, cancels SwitchLatest and DropIfBusy work, then runs alone; anything submitted while
    * it is pending or running starts after it.
    */
  case Exclusive

object Lane:
  /** The lanes [[EffectLanes]] runs: everything but [[Lane.Inline]]. */
  type Scheduled = Keyed | Exclusive.type
