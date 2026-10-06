# Offline spell checking

Serenity's spell checker (`com.serenity.spellcheck.SpellChecker`) reads standard Hunspell `.aff`/`.dic` dictionary
pairs. British English (`en-gb`) works out of the box with the dictionary bundled in the application; any other
language needs an already-installed OS dictionary, which is found automatically, or one configured by hand. This page
covers all three.

## Config keys

All spell-check settings live under `spellcheck.*` (`SpellCheckConfig`, `com.serenity.config.LanguageToolsConfig`):

| Key                          | Default | Meaning                                                                 |
| ----------------------------- | ------- | ------------------------------------------------------------------------ |
| `spellcheck.enabled`          | `false` | Turns spell-check diagnostics on.                                       |
| `spellcheck.languages`        | `["en"]`| Language codes to check against; also used to pick dictionary filenames (`en.dic`, `en_GB.dic`, `fr.dic`, ...; case is ignored). |
| `spellcheck.dictionary_paths` | `[]`    | Explicit `.dic`/`.aff` file or directory paths to load dictionaries from. |
| `spellcheck.words`            | `[]`    | Extra accepted words, on top of whatever dictionary is loaded.          |

`spellcheck.dictionary_paths` accepts either a direct path to a `.dic` (or `.aff`) file, or a directory; a directory
is searched for a file named after each configured language (`<language>.dic`, with `-`/`_` both tried, e.g.
`en-gb.dic` and `en_gb.dic`, in any letter case), falling back to the directory itself if no match is found there.

## Zero-config discovery of an installed OS dictionary

When `spellcheck.dictionary_paths` is left empty, Serenity looks for a dictionary matching `spellcheck.languages` in
the standard Hunspell/MySpell install locations for the running OS (`SpellCheckConfig.defaultOsDictionaryDirectories`,
`SpellCheckConfig.discoverDictionarySourcePaths`) before falling back to its small built-in word list:

- **Linux**: `/usr/share/hunspell`, `/usr/share/myspell/dicts`, `/usr/local/share/hunspell`,
  `/usr/local/share/myspell/dicts`
- **macOS**: `/Library/Spelling`, `~/Library/Spelling`, `/usr/share/hunspell`, `/usr/local/share/hunspell`
- **Windows**: `%PROGRAMDATA%\hunspell` (where LibreOffice installs its bundled dictionaries), when `PROGRAMDATA` is
  set

If one of these directories already has a dictionary for a configured language -- for example `hunspell-en-gb`
installed via the system package manager on Linux -- spell-check works with `spellcheck.enabled = true` and nothing
else configured. This lookup is only consulted when `spellcheck.dictionary_paths` is empty: a path you have
configured yourself is always used as configured and never silently second-guessed.

Note the default `spellcheck.languages` is `["en"]`, not `["en-gb"]`. A generic `en` dictionary (e.g. from
`hunspell-en-us`) satisfies it; British English specifically requires either an OS dictionary literally named `en.dic`
(some distributions symlink their configured default there) or adding `"en-gb"` to `spellcheck.languages`, which uses
the bundled dictionary (below) when no installed one is found.

## Bundled British English dictionary

Adding `"en-gb"` to `spellcheck.languages` and turning `spellcheck.enabled` on is all British English needs:

```
spellcheck.enabled = true
spellcheck.languages = ["en-gb"]
```

Serenity ships LibreOffice's `en_GB` Hunspell dictionary (Marco A.G.Pinto's British English, version 4.0.1, from
<https://github.com/LibreOffice/dictionaries/tree/master/en>) under `src/main/resources/spellcheck/`, together with its
upstream README. Its licence terms (LGPL) and attribution are in `THIRD-PARTY-NOTICES.md`, and the files are
unmodified copies of upstream.

- **A dictionary you supply wins.** The bundled one is used only when no dictionary named for the language (`en_GB.dic`
  or `en-gb.dic`, in any letter case) comes from `spellcheck.dictionary_paths` or an OS directory. Removing yours brings
  the bundled one back.
- **It is loaded on demand.** Nothing is parsed at startup, and nothing at all while `spellcheck.enabled` is `false` or
  no `en-gb` language is configured; the first analysis after both hold parses it once and keeps it for the session.
- **`en` is not `en-gb`.** The default `["en"]` keeps its small built-in word list, so an American-English
  dictionary installed for `en` is never second-guessed.

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
   spellcheck.languages = ["en-gb"]
   spellcheck.dictionary_paths = ["/home/you/.serenity/dictionaries"]
   ```
   (A path to the `.dic` file directly also works; the `.aff` beside it is picked up automatically.)
