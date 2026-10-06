# Offline spell checking

Serenity's spell checker (`com.serenity.spellcheck.SpellChecker`) reads standard Hunspell `.aff`/`.dic` dictionary
pairs. British English (`en-gb`, and a bare `en` when nothing installed serves English) works out of the box with the
dictionary bundled in the application; any other language needs an already-installed OS dictionary, which is found
automatically, or one configured by hand. This page covers all three.

## Config keys

All spell-check settings live under `spellcheck.*` (`SpellCheckConfig`, `com.serenity.config.LanguageToolsConfig`):

| Key                          | Default | Meaning                                                                 |
| ----------------------------- | ------- | ------------------------------------------------------------------------ |
| `spellcheck.enabled`          | `true`  | Spell-check diagnostics in prose buffers.                               |
| `spellcheck.languages`        | `["en-GB"]` | Language codes to check against; also used to pick dictionary filenames (`en_GB.dic`, `en_US.dic`, `fr.dic`, ...). |
| `spellcheck.dictionary_paths` | `[]`    | Explicit `.dic`/`.aff` file or directory paths to load dictionaries from. |
| `spellcheck.words`            | `[]`    | Extra accepted words, on top of whatever dictionary is loaded.          |

Language codes are read in any casing and with `-` or `_` (`en-gb`, `en_GB`, `EN-gb`) and kept as `en-GB`. Select
American English with `spellcheck.languages = ["en-US"]`.

`spellcheck.dictionary_paths` accepts either a direct path to a `.dic` (or `.aff`) file, or a directory; a directory
is searched for a file named after each configured language, trying the conventional Hunspell spelling first
(`en_GB.dic`) and then `en-GB.dic`, `en_gb.dic`, `en-gb.dic` and the upper-case forms, in any letter case, falling
back to the directory itself if no match is found there.

## Zero-config discovery of an installed OS dictionary

When `spellcheck.dictionary_paths` is left empty, Serenity looks for a dictionary matching `spellcheck.languages` in
the standard Hunspell/MySpell install locations for the running OS (`SpellCheckConfig.defaultOsDictionaryDirectories`,
`SpellCheckConfig.discoverDictionarySourcePaths`) before falling back to the bundled British English dictionary (for
`en-gb` and a bare `en`) or to the missing-dictionary notice:

- **Linux**: `/usr/share/hunspell`, `/usr/share/myspell/dicts`, `/usr/local/share/hunspell`,
  `/usr/local/share/myspell/dicts`
- **macOS**: `/Library/Spelling`, `~/Library/Spelling`, `/usr/share/hunspell`, `/usr/local/share/hunspell`
- **Windows**: `%PROGRAMDATA%\hunspell` (where LibreOffice installs its bundled dictionaries), when `PROGRAMDATA` is
  set

If one of these directories already has a dictionary for a configured language -- for example `hunspell-en-gb`
installed via the system package manager on Linux, which installs `/usr/share/hunspell/en_GB.dic` -- spell-check works
with nothing configured. Each language loads from the first directory that has it, so a dictionary linked into
several of these directories is loaded once. This lookup is only consulted when `spellcheck.dictionary_paths` is
empty: a path you have configured yourself is always used as configured and never silently second-guessed.

When no dictionary resolves for a configured language, every prose buffer shows one notice on its first line naming
that language and the directories searched, and no words are flagged for it -- checking against a near-empty word list
would flag every word. This holds for every language without a dictionary (`fr`, `el`, `en-US` and so on). Only British
English and a bare `en` never reach it: they have a bundled dictionary (below). The notice names only the languages
still unserved, and prose in the languages that did resolve keeps being checked.

A config file saved by an earlier version keeps the values it was saved with (`spellcheck.enabled = false`,
`spellcheck.languages = ["en"]`); a saved `["en"]` is checked against the bundled British English dictionary unless an
`en` dictionary is installed. `en` looks for `en.dic`, which Debian and Ubuntu do not install, and does not pick up an
installed `en_US.dic`: select American English with `spellcheck.languages = ["en-US"]`.

## What is checked

Spell check applies to prose buffers. Within them it checks the prose and leaves alone what only sits next to it:
fenced code blocks, inline code, HTML tags, URLs, email addresses, link targets and front matter. A word is accepted if
the dictionary has it or an affixed form of it, if every part of a hyphenated word (`well-known`, `mother-in-law`) is
accepted, or if every part either side of an apostrophe is (`o'clock`, `author's`). A typographic apostrophe is the same
letter as a straight one. Words with a digit in them (`1990s`, `19th-century`, `COVID-19`), all-capitals acronyms and
capitalised words that do not start a sentence (probably names) are not checked. Only the wrong part of a hyphenated
word is marked.

## Corrections

Right-click a misspelled word for a menu that leads with up to five corrections, then **Add to Dictionary**, **Ignore
Once** and **Ignore All**. From the keyboard, the command palette offers **Show Spelling Suggestions** (the same menu,
below the word at the cursor), **Ignore Misspelling Once** and **Ignore Misspelling Everywhere**. Choosing a correction
replaces the word as one undoable edit.

Corrections are searched for when asked for, never during the background check, so typing is not slowed by them. They
come from the dictionary's `REP` table anywhere in the word, every single edit (a swapped, missing, extra or wrong
letter) that is a word, dictionary stems within two edits, a stem within two edits of the word less a suffix with the
suffix put back (`jugdment` to `judgement`), and a split into two words (`alot` to `a lot`). They are ranked by edit
distance, with `REP` matches first, match the typo's capitalisation, and leave out words the dictionary marks
`NOSUGGEST`.

**Add to Dictionary** appends the word to `spellcheck.words` and saves the configuration, so it is accepted in every
later session. **Ignore Once** hides that occurrence, and **Ignore All** hides the word wherever it appears (in any
case), for the rest of the session only. An ignored occurrence shows again if the text at that place changes.

## How a dictionary is held

A dictionary stays as the stems in its `.dic` file. Affixes are stripped when a word is looked up, so memory follows the
number of entries rather than the number of forms they generate, and a suffix that grants a second suffix (a
continuation class) is understood without enumerating the combinations. Flag sets are shared between entries, and the
index suggestions use is built the first time one is requested.

## Bundled British English dictionary

British English needs nothing configured: `spellcheck.enabled` and `spellcheck.languages = ["en-GB"]` are the defaults.
A config file that predates them needs:

```
spellcheck.enabled = true
spellcheck.languages = ["en-GB"]
```

Serenity ships LibreOffice's `en_GB` Hunspell dictionary (Marco A.G.Pinto's British English, version 4.0.1, from
<https://github.com/LibreOffice/dictionaries/tree/master/en>) under `src/main/resources/spellcheck/`, together with its
upstream README. Its licence terms (LGPL) and attribution are in `THIRD-PARTY-NOTICES.md`, and the files are
unmodified copies of upstream.

- **A dictionary you supply wins.** The bundled one is used only when no dictionary named for the language (`en_GB.dic`
  or `en-gb.dic`, in any letter case) comes from `spellcheck.dictionary_paths` or an OS directory. Removing yours brings
  the bundled one back.
- **It is loaded on demand.** Nothing is parsed at startup, and nothing at all while `spellcheck.enabled` is `false` or
  neither `en-gb` nor `en` is configured; the first analysis after both hold parses it once and keeps it for the session.
- **A bare `en` falls back to it.** Dictionary precedence for English, highest first:
  1. A dictionary from `spellcheck.dictionary_paths`, or found in an OS directory, named for the configured language
     (`en_US.dic` for `en-US`, `en_GB.dic` for `en-gb`, `en.dic` for `en`).
  2. For a bare `en` only: the bundled British English dictionary, used when no supplied dictionary is named `en` or
     `en-*` and none is named for a language you did not configure (a bare `words.dic`, which may be serving anything).
  3. Otherwise, the missing-dictionary notice above, with nothing flagged for that language.

  So `["en-US"]` with `en_US.dic` installed uses that dictionary, and `["en-US"]` with none installed shows the notice
  rather than quietly checking American prose against British spellings.
- **Other languages have no bundled dictionary.** `fr` and `el` carry a built-in word list of a handful of words, far too
  small to check prose with, so they follow the notice rule instead of flagging every ordinary word.

## Setting up another language (or British English by hand)

Prepare this *before* going offline, since it requires downloading a dictionary:

1. Obtain a `.aff` / `.dic` pair, e.g. `en_GB.aff` / `en_GB.dic`. Common sources:
   - Your OS package manager, e.g. `apt install hunspell-en-gb` on Debian/Ubuntu (installs into
     `/usr/share/hunspell`, where the zero-config discovery above will find it automatically), or the equivalent
     `myspell-en-gb` / `hunspell-en-GB` package on other distributions.
   - LibreOffice's dictionaries (bundled with any LibreOffice install, or downloadable separately from its
     extensions repository).
   - The upstream Hunspell dictionaries project.
2. Place the pair together in one directory, e.g. `~/.serenity/dictionaries/en_GB.aff` and `en_GB.dic`.
3. Configure Serenity (`~/.serenity/config.conf`, or the in-app settings command runner):
   ```
   spellcheck.enabled = true
   spellcheck.languages = ["en-GB"]
   spellcheck.dictionary_paths = ["/home/you/.serenity/dictionaries"]
   ```
   (A path to the `.dic` file directly also works; the `.aff` beside it is picked up automatically.)
