package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1864: every commit leaves the paned buffers' indexes current, so the copies the next event makes (and the frames
  * rendered from them) read the committed index instead of rebuilding it.
  */
class CommitBufferIndexesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "prepareCommit" should "commit a state whose paned buffer indexes later copies reuse" in {
    val state = AppState.initial
    val committed =
      StateManagerOperationBoundary.prepareCommit(state, state).fold(errors => fail(errors.mkString), identity)
    val copied = committed.copy(runtime = committed.runtime.observeTyping(1L))

    copied.annotationIndex(BufferId(0)).getOrElse(fail("no annotation index")) should be theSameInstanceAs
      committed.annotationIndex(BufferId(0)).getOrElse(fail("no annotation index"))
    copied.semanticTokensAvailability(BufferId(0)).getOrElse(fail("no semantic tokens")) should be theSameInstanceAs
      committed.semanticTokensAvailability(BufferId(0)).getOrElse(fail("no semantic tokens"))
  }
