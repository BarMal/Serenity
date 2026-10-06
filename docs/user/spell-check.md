# Spell check

Spell check is on by default, in British English, and works offline. It checks prose buffers against Hunspell
dictionaries, and Serenity ships the British English one, so nothing needs installing for `en-GB`.

Misspelled words are drawn with a squiggle. Right-click one for up to five corrections, then **Add to Dictionary**,
**Ignore Once** and **Ignore All**. From the keyboard, the command palette has **Show Spelling Suggestions**,
**Ignore Misspelling Once**, **Ignore Misspelling Everywhere** and **Add Word to Dictionary**.

## Settings

In `~/.serenity/config.conf`:

```
spellcheck.enabled = true
spellcheck.languages = ["en-GB"]
spellcheck.dictionary_paths = []
spellcheck.words = []
```

- `spellcheck.languages`: language codes may be written `en-gb`, `en_GB` or `EN-gb`. For American English use
  `["en-US"]`. List several to check against several.
- `spellcheck.words`: words to accept. **Add to Dictionary** appends to this list.
- `spellcheck.dictionary_paths`: a folder, or a `.dic` file with its `.aff` beside it. When it is set, Serenity uses
  exactly what you give it. When it is empty, Serenity searches the folders below.

The same settings are under **Spell Check** in the command palette's settings. Spell check is off in
[safe mode](safe-mode.md).

## Other languages and installed dictionaries

British English and the words you add need no setup. Any other language needs a Hunspell dictionary installed. Serenity
looks for `<language>.dic` (for example `fr.dic` or `en_US.dic`) in the first of these folders that has it:

| OS | Folders |
| --- | --- |
| Linux | `/usr/share/hunspell`, `/usr/share/myspell/dicts`, `/usr/local/share/hunspell`, `/usr/local/share/myspell/dicts` |
| macOS | `/Library/Spelling`, `~/Library/Spelling`, `/usr/share/hunspell`, `/usr/local/share/hunspell` |
| Windows | `%PROGRAMDATA%\hunspell` (where LibreOffice installs its dictionaries) |

On Debian and Ubuntu, for example, the `hunspell-fr` package puts `fr.dic` in `/usr/share/hunspell`.

Windows has no standard Hunspell folder. If yours is elsewhere, set `spellcheck.dictionary_paths`:

```
spellcheck.dictionary_paths = ["C:\\Dictionaries"]
```

If a language you configured has no dictionary, Serenity shows one notice per session naming the folders it searched
and suggesting a package. Nothing is flagged for that language until a dictionary is found.

## What is checked

Prose only. Code blocks, inline code, HTML tags, URLs, email addresses, link targets and front matter are skipped, and
so are words with digits, all-capitals acronyms and capitalised words in mid-sentence (probably names). Hyphenated
words are checked part by part.

**Add to Dictionary** is kept for every later session. **Ignore Once** and **Ignore All** last until you quit.

The full detail, including how the bundled dictionary is licensed and how corrections are found, is in
[docs/spell-check.md](../spell-check.md).
