# IME platform matrix

Evidence-gathering for issue #2008 (no IME pre-edit, so CJK users cannot type). Nothing in the editor consumes
input-method events yet; this page says what to measure on each platform before the composition model is built on it.

## Turning the diagnostic on

```bash
SERENITY_IME_DIAGNOSTICS=1 serenity
```

With the variable unset the canvas has no input-method listener and no `InputMethodRequests`, so behaviour is
unchanged. With it set, the windowed interface logs one line per event to the normal log (logger `ImeDiagnostics`, level
INFO):

```
t=1760900000000 IME id=INPUT_METHOD_TEXT_CHANGED committed=2 length=4 caret=3 leading visible=none codePoints=[3053,3093,306B,3061] runs=[[0,2) plain; [2,4) CONVERTED selected]
t=1760900000001 KEY_TYPED char=3053 keyCode=0 modifiers=0
t=1760900000002 REQ getTextLocation offset=3 leading
```

Fields: `t` wall-clock millis; `id` the AWT event id; `committed` the event's committed character count; `length` the
whole text including the committed prefix; `caret` and `visible` as character index plus edge; `codePoints` hex, one
per code point, so astral characters show as one; `runs` the attribute runs, with the input-method highlight state
(`RAW`/`CONVERTED`) and whether the run is the selected clause. Text is never logged as characters.

The `REQ` lines record which `InputMethodRequests` methods the platform actually calls. The requests object reports an
empty document and places the candidate window at the canvas origin: it exists to see what the platform asks for, not
to position anything.

## What to test, on each platform

Use a Japanese (Hiragana to Kanji) input method as the main case, and a Chinese Pinyin one for a second opinion.

1. Type `kanji` and convert to 漢字; check the events seen while composing, on conversion, and on commit.
2. Type a few characters, then move the editor cursor with the mouse before committing.
3. Type a few characters, then press Escape.
4. Type a few characters, then switch focus away from the window (click another application).
5. Commit a character outside the Basic Multilingual Plane (for example 𠮷, U+20BB7).
6. Switch input method while composing.
7. Hold a key that auto-repeats with the input method off, as a control.

## Matrix

Fill in one column per platform. Record the JDK version and vendor.

| Question | macOS | Windows | Linux IBus | Linux Fcitx |
| --- | --- | --- | --- | --- |
| Does `KEY_TYPED` carry committed text? | | | | |
| Which XIM style does the JDK pick? (X11 only) | n/a | n/a | | |
| Does `endComposition` commit or discard? | | | | |
| Does the platform stop drawing its own pre-edit once `InputMethodRequests` is non-null? | | | | |
| Which `REQ` methods are called, and how often? | | | | |
| Is the committed astral character delivered as one event or as two surrogate halves? | | | | |
| Does a commit after a cursor move arrive with a fresh `INPUT_METHOD_TEXT_CHANGED`? | | | | |

## Open questions

- **Does `KEY_TYPED` carry committed text?** If it does, committed text arrives twice, once as `KEY_TYPED` and once in the
  input-method event, and the editor must take only one. The log shows both lines for a single commit.
- **Which XIM style does the JDK pick?** On X11 the JDK chooses between on-the-spot, over-the-spot, off-the-spot and root
  window styles. Only the on-the-spot style delivers pre-edit text to the application; the others draw it in a separate
  platform window. Record which style is in use and whether `-Dawt.im.style=onthespot` changes the result.
- **Does `endComposition` commit or discard?** Observe what the platform does with uncommitted pre-edit text when the
  window loses focus or the input method is switched, and whether a final commit event precedes it.
- **Do platforms stop drawing their own pre-edit once `InputMethodRequests` is non-null?** Run each test with the flag
  off (the platform's own pre-edit window, or nothing) and on, and compare.

## Linux set-up notes

IBus: `GTK_IM_MODULE=ibus XMODIFIERS=@im=ibus`, daemon started with `ibus-daemon -drx`. Fcitx: `XMODIFIERS=@im=fcitx`.
The issue's acceptance run is IBus on the X11 toolkit (`SERENITY_TOOLKIT`).
