package com.serenity.build

import java.nio.file.Path

import com.serenity.lsp.model.Diagnostic
import com.serenity.trust.CommandSource

/** Names a build provider ("command-table", "bsp", ...). Language- and tool-neutral by design: nothing in this package
  * may name a particular build tool (#2526).
  */
opaque type BuildProviderId = String

object BuildProviderId:
  def apply(value: String): BuildProviderId = value

  extension (id: BuildProviderId) def value: String = id

enum TaskKind:
  case Build, Test, Run, Debug, Dependencies

final case class BuildTargetRef(provider: BuildProviderId, id: String, label: String, kinds: Set[TaskKind])

/** `version` orders requests for one root: a newer one supersedes any older one still running. */
final case class TaskRequest(kind: TaskKind, targets: List[BuildTargetRef], version: Long)

enum OutputChannel:
  case Stdout, Stderr, Log

enum TaskOutcome:
  case Succeeded
  case Failed(summary: String)
  case Cancelled
  case Unavailable(reason: String)

enum TaskEvent:
  case Started(taskId: String, label: String)
  case Progress(taskId: String, message: String, done: Option[Long], total: Option[Long])
  case Output(channel: OutputChannel, text: String)
  case Diagnostics(file: Path, target: Option[BuildTargetRef], reset: Boolean, items: List[Diagnostic])
  case TestResult(target: BuildTargetRef, passed: Int, failed: Int, skipped: Int)
  case Finished(outcome: TaskOutcome)

final case class ProviderCapabilities(kinds: Set[TaskKind], canCancel: Boolean, listsTargets: Boolean, debug: Boolean)

/** What a provider found at a root, before anything has been started. `command` and `source` are what trust is asked
  * about, and what a trust prompt shows.
  */
final case class BuildCandidate(
    provider: BuildProviderId,
    root: Path,
    label: String,
    command: List[String],
    source: CommandSource
)

enum Detection:
  case Found(candidate: BuildCandidate)
  case Stale(candidate: BuildCandidate, reason: String)

/** Where a debug adapter can be reached, for the DAP client (#2458). */
final case class DapEndpoint(uri: String)
