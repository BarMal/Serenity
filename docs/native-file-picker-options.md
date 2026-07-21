# Native File Picker Options

## Current boundary

Serenity routes open/save/save-as through `com.serenity.io.FileDialog`, with workflow coverage already living at the `StateManager` boundary. On Windows, the concrete desktop implementation uses the Vista+ Common Item Dialog (`IFileOpenDialog` / `IFileSaveDialog`) when Serenity has a usable top-level AWT owner window. It falls back to AWT `FileDialog` if that COM API is unavailable, and to Swing `JFileChooser` when no usable owner window exists.

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
- Windows-only, so macOS and Linux retain AWT `FileDialog`
- needs a JNA bridge and COM lifecycle handling

## Decision for this slice

On Windows, prefer the Common Item Dialog when Serenity has a native-capable owner window. If COM initialization, dialog creation, or selection retrieval fails, fall back to AWT `FileDialog`. On macOS and Linux, prefer AWT `FileDialog`; fall back to `JFileChooser` when no owner is available.

This provides Windows 11-style file management while retaining native locations, permissions, keyboard behavior, and cancellation semantics. It preserves the existing open/save/save-as/cancel workflow tests at the abstraction boundary.
