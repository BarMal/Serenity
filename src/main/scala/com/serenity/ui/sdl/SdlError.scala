package com.serenity.ui.sdl

/** Why an SDL operation did not happen. Nothing in the binding throws: the caller decides whether to fall back. */
enum SdlError:
  /** No candidate library opened; one reason per candidate, in the order they were tried. */
  case LibraryUnavailable(failures: List[String])

  /** The library opened but lacks a function the binding needs, as a build of another SDL major version would. */
  case MissingFunction(name: String)

  /** An SDL function reported failure; `message` is what `SDL_GetError` said. */
  case Call(function: String, message: String)

  def describe: String = this match
    case LibraryUnavailable(failures) => s"libSDL3 did not load (${failures.mkString("; ")})"
    case MissingFunction(name)        => s"libSDL3 has no $name"
    case Call(function, message)      => s"$function failed: $message"
