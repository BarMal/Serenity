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

The file name is the language code with `.dic` on the end, in any letter case and with `-` or `_`: `fr` finds
`fr.dic`, and `en-US` finds `en_US.dic`. The `.aff` file must be beside it.

Windows has no standard Hunspell folder. If yours is elsewhere, set `spellcheck.dictionary_paths`:

```
spellcheck.dictionary_paths = ["C:\\Dictionaries"]
```

If no dictionary at all can be found for the languages you configured, Serenity shows one notice per session, when
there is prose to check. It names the folders searched and suggests installing a package called `hunspell-` followed
by your first language in lower case (`hunspell-fr`). Nothing is flagged until a dictionary is found. If only some of
your languages have one, there is no notice, and the others are not checked.

## What is checked

Prose only. Code blocks, inline code, HTML tags, URLs, email addresses, link targets and front matter are skipped, and
so are words with digits, all-capitals acronyms, and capitalised words that are not the first word of a sentence
(probably names). A sentence starts at the beginning of a line or after `.`, `!` or `?`. Hyphenated
words are checked part by part.

**Add to Dictionary** is kept for every later session. **Ignore Once** and **Ignore All** last until you quit.

The full detail, including how the bundled dictionary is licensed and how corrections are found, is in
[docs/spell-check.md](../spell-check.md).
