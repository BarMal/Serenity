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

## Choosing a folder

`FileDialog.chooseFolder` asks for a directory, and the backend is picked per platform because the AWT dialog only picks directories on macOS:

- Windows: the Common Item Dialog with `FOS_PICKFOLDERS`. If COM fails it falls back to `JFileChooser`, not to the AWT dialog, which cannot pick a folder there.
- macOS: the AWT `FileDialog` with `apple.awt.fileDialogForDirectories` set to `true`. The property is JVM-wide, so every macOS AWT dialog sets it explicitly for its own duration (`false` for file dialogs) and restores the previous value in a `finally`. Dialogs run on the event dispatch thread, where a second dialog can only start inside the first one's modal loop, so the changes nest and cannot interleave.
- Linux, and any system with no usable owner window: `JFileChooser` with `DIRECTORIES_ONLY`. The Linux AWT dialog has no directory mode.

With no native dialog (the terminal), Open folder shows the in-app Open Folder form: the Path field and the folders under it, with files hidden because none is a valid choice (the same rule Save As uses for its location). `Enter` browses into the folder the Path names, `Tab` descends into the highlighted one, and the confirm key (`Ctrl+R`, the same action the generic Open form labels "Open as root") opens the shown folder through the same route as a native dialog's choice.

## Choosing a file or a folder (macOS)

macOS has one Open... that takes either, as VS Code, Zed and Pulsar do. `FileDialog.chooseFileOrFolder` is backed by `supportsFileOrFolder`, which is true only on macOS and only if the native probe succeeds; elsewhere it is false and the start page, palette and menu keep Open file and Open folder.

- Why not a library or AWT: the AWT `FileDialog` cannot do it, because OpenJDK's `CFileDialog.m` sets `setCanChooseFiles:!fChooseDirectories` and `setCanChooseDirectories:fChooseDirectories` from one flag. NFD-extended has no mixed mode on any platform. Zed calls `NSOpenPanel` directly (`crates/gpui_macos/src/platform.rs`); VS Code branches on `isMacintosh` (`workspaceActions.ts`, `gettingStartedContent.ts`).
- Mechanism: JNA, the same FFI as the Windows Common Item Dialog (`jna-platform` 5.19.1). `OpenPanelScript` sends `[NSOpenPanel openPanel]`, `setCanChooseFiles:YES`, `setCanChooseDirectories:YES`, `setAllowsMultipleSelection:NO`, `setDirectoryURL:`, `runModal`, then `URL`, `path` and `UTF8String`, inside an `NSAutoreleasePool`. Only `JnaObjectiveC` (`objc_getClass`, `sel_registerName`, `objc_msgSend`) and `CoreFoundationMainLoop` touch the runtime.
- Threading: AppKit requires the main thread. The call is made from a blocking-pool thread, never the event dispatch thread, and handed to the main run loop through a `CFRunLoopSource` that is added to the modes the JDK uses for main-thread work in `ThreadUtilities.m` (default, modal panel, event tracking and `AWTRunLoopMode`). The JDK does the same: `CFileDialog` runs `nativeRunFileDialog` on a thread of its own and waits for `safeSaveOrLoad` on the main thread. The event thread stays free, and the panel has no delegate, so AppKit never calls back into Java while it is up (the JDK's panel does, through `panel:shouldEnableURL:`); an AWT call from the event thread into AppKit is served from the panel's modal loop because the modal mode is in the list. The panel is application-modal in AppKit, so the window behind it takes no input.
- A hand-off that the main run loop never starts is abandoned after ten seconds, and the work is withdrawn so no panel can appear late. Once it has started the wait has no limit.
- Failures: if the Objective-C runtime or CoreFoundation cannot be loaded, `supportsFileOrFolder` is false and the split Open file and Open folder are used. If a call fails after that, the IO fails and the usual "Couldn't show the file dialog" notice appears.
- Routing: a directory goes to the folder route (`openFolderAsProjectRoot`), anything else to `loadFile`, decided with `Files.isDirectory` on the choice.
- The palette keeps Open File... and Open Folder... on every platform so a keyboard user can be explicit, and lists Open... only where `supportsFileOrFolder` holds.

## Decision for this slice

On Windows, prefer the Common Item Dialog when Serenity has a native-capable owner window. If COM initialization, dialog creation, or selection retrieval fails, fall back to AWT `FileDialog`. On macOS and Linux, prefer AWT `FileDialog` for files; fall back to `JFileChooser` when no owner is available. Folders follow the table above.

This provides Windows 11-style file management while retaining native locations, permissions, keyboard behavior, and cancellation semantics. It preserves the existing open/save/save-as/cancel workflow tests at the abstraction boundary.
