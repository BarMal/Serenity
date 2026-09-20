# Offline spell checking

Serenity's spell checker (`com.serenity.spellcheck.SpellChecker`) reads standard Hunspell `.aff`/`.dic` dictionary
pairs. It ships with no dictionary of its own; getting real, offline-capable spell checking working means either an
already-installed OS dictionary is found automatically, or a dictionary is configured by hand. This page covers both.

## Config keys

All spell-check settings live under `spellcheck.*` (`SpellCheckConfig`, `com.serenity.config.LanguageToolsConfig`):

| Key                          | Default | Meaning                                                                 |
| ----------------------------- | ------- | ------------------------------------------------------------------------ |
| `spellcheck.enabled`          | `false` | Turns spell-check diagnostics on.                                       |
| `spellcheck.languages`        | `["en"]`| Language codes to check against; also used to pick dictionary filenames (`en.dic`, `en_gb.dic`, `fr.dic`, ...). |
| `spellcheck.dictionary_paths` | `[]`    | Explicit `.dic`/`.aff` file or directory paths to load dictionaries from. |
| `spellcheck.words`            | `[]`    | Extra accepted words, on top of whatever dictionary is loaded.          |

`spellcheck.dictionary_paths` accepts either a direct path to a `.dic` (or `.aff`) file, or a directory; a directory
is searched for a file named after each configured language (`<language>.dic`, with `-`/`_` both tried, e.g.
`en-gb.dic` and `en_gb.dic`), falling back to the directory itself if no match is found there.

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
(some distributions symlink their configured default there) or adding `"en-gb"` to `spellcheck.languages` explicitly.

## Setting up British English (`en-GB`) for offline use, manually

Prepare this *before* going offline, since it requires downloading a dictionary:

1. Obtain an `en_GB.aff` / `en_GB.dic` pair. Common sources:
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

## Bundling a dictionary with Serenity

Shipping an `en_GB.aff`/`.dic` pair inside `src/main/resources` (alongside the existing `fonts/`, `icons/`, `sprites/`
and `themes/` bundled assets) would remove the need for step 1 above entirely for the common case. This has not been
done yet: real Hunspell dictionaries are third-party, typically LGPL/MPL/BSD-family licensed depending on the
specific dictionary, and picking one and bundling it correctly (attribution, license file, verifying redistribution
terms permit shipping inside Serenity's own distributable) needs a deliberate choice of source and review, not an
automated one. See the discussion on issue #1175 for the current state of that decision.
