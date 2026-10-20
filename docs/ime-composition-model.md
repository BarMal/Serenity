# IME composition model (issue #2008, PR 1): Phase 1 findings

Status: implemented in PR 1 on the decisions recorded below (accepted by the owner on #2008).

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

## What PR 1 builds

- `Composition` and `CompositionClause` (`state/models/Composition.scala`) and `Runtime.composition` / `Runtime.compositionRefused`.
- `ImeComposing`, `ImeCommit`, `ImeCancelled` (`keystroke/events/TextEntryEvent.scala`), reduced by `ImeReducer`.
- `ImeEventTranslation`: an AWT `InputMethodEvent` (committed prefix, then pre-edit) becomes a commit followed by the composition that goes on; `SwingInputHandler.submitInputMethodEvent` queues the result in order with the keys.
- `AppEffect.Ime(ImeEffect.Refused)`: shows `ImeNotices.refused` and calls `EffectRuntimePort.endInputMethodComposition`,
  which the windowed runtime wires to `InputContext.endComposition()`.
- The canvas listens for `InputMethodEvent`s and answers `InputMethodRequests` (`ImeRequests`): the candidate window goes
  at the primary caret, and no committed text is offered for reconversion or cancellation.

## How the pre-edit is drawn

While `Runtime.composition` holds a pre-edit that began against the buffer's current text, the windowed renderer is
handed `CompositionPreview.displayed(state)`: a copy of the buffer with the pre-edit spliced in at the anchor by the
real edit path, so layout, wrapping and carets are the real ones. The copy carries a content revision no real buffer has
(`CompositionRevisions`), so nothing that caches by revision can confuse it with the real text. `CompositionStyling`
underlines the pre-edit and puts the clause being converted on the selection background; the primary caret sits at the
composition's caret. The real state is never touched.

Known limits: the status line reads the displayed cursor while composing; a mouse click on the composing line maps
through the displayed text; syntax and spell-check marks on that line after the anchor are not shifted.

## Decisions (accepted)

1. Guard opaque blocks only; a buffer read-only flag is a separate change.
2. Refuse a composition when any cursor is on an opaque block line.
3. When refused: ignore composing events until the input method commits or cancels (the commit's text is dropped), end the composition on the platform's side, and show a notice.
4. `InsertText(String)` landed first (#2331); a commit is an `InsertText`, so it is one edit at every cursor and groups for undo like typed text.

A commit with no composition recorded inserts at the current cursor. A commit after the cursor moved goes to the recorded anchor if the buffer version still matches, otherwise to the current cursor.
