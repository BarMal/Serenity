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
  `["en-US"]` (it needs an installed `en_US` dictionary, see below). A bare `["en"]`, as saved by older versions, is
  checked as British English unless an `en` dictionary is installed. List several to check against several.
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

If any language you configured has no dictionary, Serenity shows a notice when there is prose to check, once per
session for each distinct wording. It names every language that is missing, lists the folders searched, and suggests a
package for each one, called `hunspell-` followed by the language in lower case. For example:

```
Spell check found no dictionary for fr.
Searched: /usr/share/hunspell, /usr/share/myspell/dicts, ...
Install a Hunspell dictionary (for example hunspell-fr) or set spellcheck.dictionary_paths.
```

The folders listed are your `spellcheck.dictionary_paths` when that is set, otherwise the folders in the table above.
Languages that do have a dictionary are still checked, so a missing language does not switch spell check off for the
rest. If no language has a dictionary, nothing is flagged until one is found. The tiny built-in word lists for `en`,
`fr` and `el` are too small to check prose with, so they do not count as a dictionary and do not suppress the notice.

British English never counts as missing, and neither does a bare `en`: with no `en` dictionary installed, `en` is
checked against the bundled British English one. An installed `en_US.dic` is not picked up for `en`; use
`spellcheck.languages = ["en-US"]` for American English. With `en-US` and no `en_US.dic` installed you get the notice
rather than American prose checked against British spellings.

## What is checked

Prose only. Code blocks, inline code, HTML tags, URLs, email addresses, link targets and front matter are skipped, and
so are words with digits, all-capitals acronyms, and capitalised words that are not the first word of a sentence
(probably names). A sentence starts at the beginning of a line or after `.`, `!` or `?`. Hyphenated
words are checked part by part.

**Add to Dictionary** is kept for every later session. **Ignore Once** and **Ignore All** last until you quit.

The full detail, including how the bundled dictionary is licensed and how corrections are found, is in
[docs/spell-check.md](../spell-check.md).
