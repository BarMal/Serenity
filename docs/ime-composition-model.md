# IME composition model (issue #2008, PR 1): Phase 1 findings

Status: findings and open decisions only. No model or reducer code is written until the decisions below are settled.

## Findings

Paths are relative to `src/main/scala/com/serenity/`.

1. **Runtime is the right home.** `Runtime` is "state that is never persisted" (`state/models/Runtime.scala:9-10`) and already
   holds transient editor state such as `typingActivity` and `editClock`.
2. **Everything that must stay quiet keys off the buffer.** `Buffer.contentVersion` is bumped on every `content` change
   (`state/models/Buffer.scala:114-117`); `isDirty` is a `Document` field (`Buffer.scala:100`). Composition state in
   `Runtime` touches none of them.
3. **Events share one union.** `Event = EditorEvent | AppEvent | ...`, and "a new family must be added here or its
   values are not events at all" (`keystroke/events/Event.scala:3-18`). `TextEntryEvent` is the sealed trait of editor
   text events (`keystroke/events/TextEntryEvent.scala:5-17`).
4. **Typing is reduced in two places that must both learn about composition**: `EditorEventReducer.scala:180`, `:271`,
   `:302` (including `handleEventWithoutBuffer`, which creates a buffer from the first typed char) and
   `EditorTextEditReducer.scala:45-52` (selection replacement, multi-cursor insertion, smart punctuation).
5. **Other `InsertChar` consumers** that a new event family must not accidentally fall through:
   `input/InputRouter.scala:66`, `state/components/EditorPaneComponent.scala:71`,
   `state/manager/EventPipelineTransitions.scala:57` (typing-activity stamp), `ui/widget/WidgetInput.scala:82`,
   `keystroke/events/SurfaceInput.scala:21`, `app/AppRuntimeLogging.scala:34`.
6. **Opaque regions exist and have a guard**: `OpaqueBlockGuard.refuses` runs after an edit, only when a cursor or
   selection is on or next to a block line (`state/reducers/OpaqueBlockGuard.scala:12-44`), via
   `RichTextDocument.hasOpaqueBlockBetween` (`richtext/RichTextDocument.scala:292`). It is a post-edit refusal, not a
   pre-edit check.
7. **"Read-only" has no model representation.** `Buffer` has no read-only field (`state/models/Buffer.scala`, whole
   file). The licence, notices and privacy documents are made read-only by the operating system only
   (`io/LicenceNotices.scala:30-32`, `io/PrivacyStatement.scala:30-32`, `io/AboutDocument.scala:87-89`);
   `FileUtils.isWritableFile` (`io/FileUtils.scala:15`) has no caller. `CommentLensMode.ReadOnly` is a floating peek
   that never takes focus (`document/CommentRendering.scala:30-31`), so typing already reaches the buffer behind it.
8. **Multi-cursor** is `Buffer.cursorList`; an insert at several cursors already exists as `applyMultiCursorInsertion`
   (`state/reducers/EditorTextEditReducer.scala:47,63`). Typing groups for undo come from `typingGrouping`
   (`state/reducers/EditorEditSupport.scala:49`).

## Proposed changes (once decisions are settled)

1. `Composition` (text, clauses with selected flag, caret, anchor = pane, buffer, cursor position, buffer version) and
   `Runtime.composition: Option[Composition]`.
2. `ImeComposing`, `ImeCommit`, `ImeCancelled` as a `TextEntryEvent` family, queued by the handler through the same
   queue as keys.
3. An `ImeReducer`: composing sets/replaces the composition; cancel clears it; commit clears it and inserts at the
   resolved target (anchor if the buffer version still matches, else the primary cursor), at every cursor.
4. Guard evaluated when a composition would start.
5. Specs pinning that rope, `contentVersion`, `isDirty`, undo, LSP and auto-save effects are unchanged until commit.

## Decisions needed

1. **What is "read-only"?** The brief says no composition starts in read-only or opaque regions, but only opaque blocks
   exist in the model (finding 6, 7).
   - A. Guard opaque blocks only. Read-only documents stay as today (typing is allowed; the save fails on disk).
   - B. Also add a buffer read-only flag set when a file is opened and not writable, and refuse composition in it. This
     also changes plain typing (a user-visible behaviour change and its own PR).
   - Recommendation: A, with the guard a single function so B can be added later.
2. **Which cursors does the opaque check cover?** Pre-edit shows at the primary cursor but the commit applies at all.
   - A. Refuse when any cursor's line is an opaque block line.
   - B. Refuse only for the primary cursor.
   - C. Mirror `OpaqueBlockGuard` (also the lines next to each cursor).
   - Recommendation: A. A commit that the post-edit guard would refuse should not have begun composing; adjacent lines are
     only a problem for edits that join lines, which text does not.
3. **What happens to input after an immediate end?** The platform keeps sending events for a composition we refused.
   - A. Ignore `ImeComposing` until the platform's own `ImeCommit`/`ImeCancelled`, and drop that commit's text.
   - B. Ignore composing but insert nothing and show a notice that the position is not editable.
   - C. Also call `InputContext.endComposition()` from the handler (needs a side-effect seam outside the reducer).
   - Recommendation: A plus C's call for the platform's sake, with B's notice. Silently dropping text is user-visible.
4. **Ordering against PR 2.** `ImeCommit` needs a way to insert a string at several cursors as one edit; that is
   `InsertText(String)`, which PR 2 adds. Without it a commit would be inserted char by char (two edits and a split
   surrogate pair for an astral character).
   - A. Land PR 2 first, then PR 1 uses `InsertText`.
   - B. Fold `InsertText`'s event and reducer into PR 1 and leave PR 2 with the TUI decoder and the match sites.
   - C. Keep the order; PR 1's commit uses per-code-unit `InsertChar`, with no producer wired in until PR 2.
   - Recommendation: A.
3a. **Commit with no composition** (a platform that commits directly, for example dead keys): insert at the current
   cursor as a normal `InsertText`. This is assumed, not asked; say if it should differ.
