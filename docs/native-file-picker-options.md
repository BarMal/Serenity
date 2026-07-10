# Native File Picker Options

## Current boundary

Serenity routes open/save/save-as through `com.serenity.io.FileDialog`, with workflow coverage already living at the `StateManager` boundary. The concrete desktop implementation now prefers AWT `FileDialog` and falls back to Swing `JFileChooser` when Serenity does not have a usable top-level AWT owner window.

## Options considered

### 1. `javax.swing.JFileChooser`

- Pros:
  - already in use
  - no extra dependencies or toolkit bridge
  - works anywhere Swing works
- Cons:
  - looks and feels like a Swing component rather than a modern OS file picker
  - is the weakest fit for the Windows-specific problem that opened `#427`

### 2. `java.awt.FileDialog`

- Pros:
  - built into the JDK
  - uses the AWT dialog path instead of embedding a Swing chooser component
  - keeps one cross-platform implementation for Windows, macOS, and Linux
  - lets Serenity keep the existing `FileDialog` trait and test surface
- Cons:
  - requires a `Frame` or `Dialog` owner, so a fallback is still needed
  - title handling is platform-dependent
  - filename filters are not reliable on Windows

### 3. `javafx.stage.FileChooser`

- Pros:
  - also exposes standard platform file dialogs
  - richer built-in file-type filter API than AWT
- Cons:
  - would add JavaFX modules and toolkit lifecycle concerns to a Swing/AWT app
  - the current build does not ship JavaFX

### 4. Windows Common Item Dialog (`IFileOpenDialog` / `IFileSaveDialog`)

- Pros:
  - best Windows-specific native integration path
  - exposes the newest Windows dialog capabilities directly
- Cons:
  - Windows-only
  - would require a native bridge layer such as JNA/JNI plus separate macOS/Linux strategies
  - much larger scope than the current file-dialog abstraction needs

## Decision for this slice

Prefer AWT `FileDialog` when Serenity already has a native-capable owner window. Fall back to `JFileChooser` otherwise.

This improves the Windows experience without introducing a new UI toolkit or a platform-specific native bridge, and it preserves the existing open/save/save-as/cancel workflow tests at the abstraction boundary.
