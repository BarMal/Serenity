package com.serenity.ui.sdl

import java.lang.foreign.{FunctionDescriptor, Linker, MemoryLayout, SymbolLookup, ValueLayout}
import java.lang.invoke.MethodHandle

import scala.jdk.OptionConverters.*

private val Bool: MemoryLayout    = ValueLayout.JAVA_BOOLEAN
private val Int32: MemoryLayout   = ValueLayout.JAVA_INT
private val Int64: MemoryLayout   = ValueLayout.JAVA_LONG
private val Float32: MemoryLayout = ValueLayout.JAVA_FLOAT
private val Pointer: MemoryLayout = ValueLayout.ADDRESS

/** The SDL3 functions the binding calls, each with the C signature it is linked against. `SDL_bool` is C99 `bool` (one
  * byte); `SDL_InitFlags`, `SDL_DisplayID` and `SDL_Keycode` are `Uint32`, `SDL_WindowFlags` is `Uint64`.
  */
private[sdl] enum SdlFunction(val symbol: String, val descriptor: FunctionDescriptor):
  case Init                  extends SdlFunction("SDL_Init", FunctionDescriptor.of(Bool, Int32))
  case Quit                  extends SdlFunction("SDL_Quit", FunctionDescriptor.ofVoid())
  case GetError              extends SdlFunction("SDL_GetError", FunctionDescriptor.of(Pointer))
  case GetVersion            extends SdlFunction("SDL_GetVersion", FunctionDescriptor.of(Int32))
  case SetHint               extends SdlFunction("SDL_SetHint", FunctionDescriptor.of(Bool, Pointer, Pointer))
  case GetCurrentVideoDriver extends SdlFunction("SDL_GetCurrentVideoDriver", FunctionDescriptor.of(Pointer))
  case Free                  extends SdlFunction("SDL_free", FunctionDescriptor.ofVoid(Pointer))
  case CreateWindow
      extends SdlFunction("SDL_CreateWindow", FunctionDescriptor.of(Pointer, Pointer, Int32, Int32, Int64))
  case DestroyWindow  extends SdlFunction("SDL_DestroyWindow", FunctionDescriptor.ofVoid(Pointer))
  case ShowWindow     extends SdlFunction("SDL_ShowWindow", FunctionDescriptor.of(Bool, Pointer))
  case RaiseWindow    extends SdlFunction("SDL_RaiseWindow", FunctionDescriptor.of(Bool, Pointer))
  case MaximizeWindow extends SdlFunction("SDL_MaximizeWindow", FunctionDescriptor.of(Bool, Pointer))
  case MinimizeWindow extends SdlFunction("SDL_MinimizeWindow", FunctionDescriptor.of(Bool, Pointer))
  case RestoreWindow  extends SdlFunction("SDL_RestoreWindow", FunctionDescriptor.of(Bool, Pointer))
  case SetWindowTitle extends SdlFunction("SDL_SetWindowTitle", FunctionDescriptor.of(Bool, Pointer, Pointer))
  case GetWindowSize  extends SdlFunction("SDL_GetWindowSize", FunctionDescriptor.of(Bool, Pointer, Pointer, Pointer))
  case SetWindowSize  extends SdlFunction("SDL_SetWindowSize", FunctionDescriptor.of(Bool, Pointer, Int32, Int32))
  case SetWindowMinimumSize
      extends SdlFunction("SDL_SetWindowMinimumSize", FunctionDescriptor.of(Bool, Pointer, Int32, Int32))
  case GetWindowSizeInPixels
      extends SdlFunction("SDL_GetWindowSizeInPixels", FunctionDescriptor.of(Bool, Pointer, Pointer, Pointer))
  case GetWindowPixelDensity extends SdlFunction("SDL_GetWindowPixelDensity", FunctionDescriptor.of(Float32, Pointer))
  case GetWindowDisplayScale extends SdlFunction("SDL_GetWindowDisplayScale", FunctionDescriptor.of(Float32, Pointer))
  case GetWindowSurface      extends SdlFunction("SDL_GetWindowSurface", FunctionDescriptor.of(Pointer, Pointer))
  case UpdateWindowSurface   extends SdlFunction("SDL_UpdateWindowSurface", FunctionDescriptor.of(Bool, Pointer))
  case UpdateWindowSurfaceRects
      extends SdlFunction("SDL_UpdateWindowSurfaceRects", FunctionDescriptor.of(Bool, Pointer, Pointer, Int32))
  case SetWindowSurfaceVSync
      extends SdlFunction("SDL_SetWindowSurfaceVSync", FunctionDescriptor.of(Bool, Pointer, Int32))
  case GetDisplayForWindow   extends SdlFunction("SDL_GetDisplayForWindow", FunctionDescriptor.of(Int32, Pointer))
  case GetCurrentDisplayMode extends SdlFunction("SDL_GetCurrentDisplayMode", FunctionDescriptor.of(Pointer, Int32))
  case PollEvent             extends SdlFunction("SDL_PollEvent", FunctionDescriptor.of(Bool, Pointer))
  case WaitEventTimeout      extends SdlFunction("SDL_WaitEventTimeout", FunctionDescriptor.of(Bool, Pointer, Int32))
  case PushEvent             extends SdlFunction("SDL_PushEvent", FunctionDescriptor.of(Bool, Pointer))
  case StartTextInput        extends SdlFunction("SDL_StartTextInput", FunctionDescriptor.of(Bool, Pointer))
  case StopTextInput         extends SdlFunction("SDL_StopTextInput", FunctionDescriptor.of(Bool, Pointer))
  case SetTextInputArea
      extends SdlFunction("SDL_SetTextInputArea", FunctionDescriptor.of(Bool, Pointer, Pointer, Int32))
  case GetKeyName              extends SdlFunction("SDL_GetKeyName", FunctionDescriptor.of(Pointer, Int32))
  case CreateSystemCursor      extends SdlFunction("SDL_CreateSystemCursor", FunctionDescriptor.of(Pointer, Int32))
  case SetCursor               extends SdlFunction("SDL_SetCursor", FunctionDescriptor.of(Bool, Pointer))
  case DestroyCursor           extends SdlFunction("SDL_DestroyCursor", FunctionDescriptor.ofVoid(Pointer))
  case SetClipboardText        extends SdlFunction("SDL_SetClipboardText", FunctionDescriptor.of(Bool, Pointer))
  case GetClipboardText        extends SdlFunction("SDL_GetClipboardText", FunctionDescriptor.of(Pointer))
  case HasClipboardText        extends SdlFunction("SDL_HasClipboardText", FunctionDescriptor.of(Bool))
  case SetPrimarySelectionText extends SdlFunction("SDL_SetPrimarySelectionText", FunctionDescriptor.of(Bool, Pointer))
  case GetPrimarySelectionText extends SdlFunction("SDL_GetPrimarySelectionText", FunctionDescriptor.of(Pointer))
  case HasPrimarySelectionText extends SdlFunction("SDL_HasPrimarySelectionText", FunctionDescriptor.of(Bool))

private[sdl] object SdlFunction:

  /** A downcall handle for every function, or the first one the library does not have. */
  def bindAll(lookup: SymbolLookup): Either[SdlError, Map[SdlFunction, MethodHandle]] =
    val linker = Linker.nativeLinker()
    SdlFunction.values.toList.foldLeft[Either[SdlError, Map[SdlFunction, MethodHandle]]](Right(Map.empty)) {
      (bound, function) =>
        bound.flatMap(handles =>
          lookup
            .find(function.symbol)
            .toScala
            .toRight(SdlError.MissingFunction(function.symbol))
            .map(address => handles.updated(function, linker.downcallHandle(address, function.descriptor)))
        )
    }
