# Mobile Port Exploration

This exploration records the outcome for [GitHub issue #179](https://github.com/BarMal/Serenity/issues/179): mobile work remains deferred until a text-editing-only proof of concept demonstrates a viable Android and iOS interaction model.

## Decision

Caution, here be imagine dragons: the recommendation not to start a mobile
implementation here is an architectural inference from the current code and the
issue constraint. Serenity is a Scala/JVM desktop application with a
Swing/AWT/Java2D platform shell, not a cross-platform application with a
replaceable mobile shell. A direct port would therefore be a UI and
platform-services rewrite, not a packaging change.[2][3][4][5]

If mobile work is revisited, treat it as a separate, text-editing-only client. It
must not include LSP, build servers, compilation, debugging, or other IDE
workflows; that boundary is the explicit constraint of issue #179.[1]

## What Can And Cannot Be Reused

The current `sbt` build is a single Scala 3 JVM project and declares no Android,
iOS, Kotlin/Native, or Scala Native target.[2] Startup creates a `SwingWindow`,
uses `SwingInputHandler`, AWT clipboard access, and `SwingFileDialog`, then wires
those implementations into the runtime.[3] The desktop renderer constructs a
`Java2DRenderSurface` from that Swing window and its `JPanel` canvas.[4]

The runtime itself accepts input, resize, rendering, and shutdown capabilities as
parameters, which is a useful seam for a future shell.[5] `InputHandler` and
`SystemClipboard` are also abstract interfaces.[6][7] That seam is incomplete for
mobile reuse: the shared rendering API and the state, theme, font, layout, and
animation layers expose `java.awt` types; the concrete desktop implementations
also depend on Swing and AWT.[4][8][9][10]

Consequently, the editor's behaviour can inform a mobile client, but the present
Scala sources cannot be treated as a mobile-ready shared module without a large,
separate portability programme.

## Options Considered

1. **Package the existing application for mobile — reject.** Android and iOS
   cannot consume the current Swing/AWT/Java2D shell as-is.[3][4][8][9]

2. **Add a Scala Native target — defer.** The current build is JVM-only, and
   Scala Native's documented cross-compilation examples target native desktop
   triples rather than documenting Android or iOS application packaging.[2][11]
   This option would additionally require replacing the Java2D/Swing APIs that
   occur across the present rendering and display stack.[4][8][9][10]

3. **Create a separate Kotlin mobile client — preferred discovery path.**
   Compose Multiplatform is documented as stable for Android and iOS, with
   platform-specific entry points and native integrations where required.[12][13]
   It would still require a new renderer, input/IME handling, storage, and
   clipboard integrations, as well as either a reimplementation of the editor
   model or an explicitly designed interchange boundary. This is a product and
   architecture exploration, not a claim that Serenity code is directly
   reusable.

4. **Replace the desktop shell with another JVM toolkit first — reject for this
   issue.** It does not create an iOS-ready shared module and would risk the
   existing desktop editor without proving the mobile interaction model.

## Recommended Discovery Pilot

Before funding a port, build a disposable Android-and-iOS proof of concept outside
this repository. It should contain exactly one locally stored plain-text document
and prove the following:

- touch placement, drag selection, scrolling, and platform back behaviour;
- software-keyboard and hardware-keyboard text entry, including composing input;
- copy, cut, paste, undo, redo, and local-file open/save;
- a long-document performance sample using Serenity-compatible editing scenarios;
- proportional-font layout, dynamic type/text scaling, safe areas, and both
  portrait and landscape layouts; and
- accessibility semantics for the editable surface and selection actions.

Use fixed editing test vectors derived from Serenity's existing rope and editor
behaviour rather than attempting source-level sharing. The pilot succeeds only if
those interactions are reliable on a physical Android device and an iPhone or iOS
simulator, and if the resulting interaction model remains recognisably a text
editor rather than a compressed desktop UI.

The pilot should explicitly exclude sessions, themes, rich-text codecs, Markdown
preview, LSP, diagnostics, build servers, compilation, debugging, and project
browsing. Those features would obscure the question this issue asks: whether a
small, high-quality text editor is viable on both mobile platforms.[1]

## Follow-up Gate

Only after the pilot should a new issue choose between:

1. a standalone mobile product with a documented document interchange format; or
2. a long-term extraction of platform-neutral editor semantics from Serenity.

The second option must begin with behaviour tests and a deliberate replacement of
desktop Java types at the shared boundary. It should not be started as incidental
refactoring during mobile UI work.

## References

[1] [GitHub issue #179](https://github.com/BarMal/Serenity/issues/179)

[2] [`build.sbt`](../build.sbt)

[3] [`src/main/scala/Main.scala`](../src/main/scala/Main.scala)

[4] [`src/main/scala/com/serenity/ui/renderer/Renderer.scala`](../src/main/scala/com/serenity/ui/renderer/Renderer.scala)

[5] [`src/main/scala/com/serenity/app/AppRuntime.scala`](../src/main/scala/com/serenity/app/AppRuntime.scala)

[6] [`src/main/scala/com/serenity/input/InputHandler.scala`](../src/main/scala/com/serenity/input/InputHandler.scala)

[7] [`src/main/scala/com/serenity/input/SystemClipboard.scala`](../src/main/scala/com/serenity/input/SystemClipboard.scala)

[8] [`src/main/scala/com/serenity/ui/terminal/SwingWindow.scala`](../src/main/scala/com/serenity/ui/terminal/SwingWindow.scala)

[9] [`src/main/scala/com/serenity/ui/renderer/Java2DRenderSurface.scala`](../src/main/scala/com/serenity/ui/renderer/Java2DRenderSurface.scala)

[10] [`src/main/scala/com/serenity/input/SwingInputHandler.scala`](../src/main/scala/com/serenity/input/SwingInputHandler.scala)

[11] [Scala Native user guide](https://scala-native.org/en/stable/user/)

[12] [Compose Multiplatform supported-platform stability](https://kotlinlang.org/docs/multiplatform/supported-platforms.html)

[13] [Compose Multiplatform platform-specific behaviour](https://kotlinlang.org/docs/multiplatform/compose-platform-specifics.html)
