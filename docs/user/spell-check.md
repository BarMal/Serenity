# Spell check

Spell check is off until you turn it on. It works offline, from Hunspell `.dic` and `.aff` dictionary files.

## Turn it on

In `~/.serenity/config.conf`:

```
spellcheck.enabled = true
spellcheck.languages = ["en-gb"]
```

British English (`en-gb`) needs nothing else: Serenity ships a dictionary for it. The default language is `en`, which
uses a small built-in word list unless an installed dictionary named `en` is found.

For any other language, or to use your own dictionary, Serenity needs an installed Hunspell dictionary.

## Where Serenity looks for installed dictionaries

When `spellcheck.dictionary_paths` is empty, Serenity searches these folders for a dictionary named after each
language in `spellcheck.languages` (for example `fr.dic` or `en_GB.dic`):

| OS | Folders |
| --- | --- |
| Linux | `/usr/share/hunspell`, `/usr/share/myspell/dicts`, `/usr/local/share/hunspell`, `/usr/local/share/myspell/dicts` |
| macOS | `/Library/Spelling`, `~/Library/Spelling`, `/usr/share/hunspell`, `/usr/local/share/hunspell` |
| Windows | `%PROGRAMDATA%\hunspell` (where LibreOffice installs its dictionaries) |

Windows has no standard Hunspell folder. If yours is elsewhere, set `spellcheck.dictionary_paths` to the folder or to
the `.dic` file; the `.aff` file must sit beside it.

```
spellcheck.dictionary_paths = ["C:\\Dictionaries"]
```

A path you set is always used as given. A dictionary you supply wins over the bundled one.

## Your own words

List extra accepted words in `spellcheck.words`, or run **Add Word to Dictionary** from the palette with the cursor on the
word.

Spell check is off in [safe mode](safe-mode.md). The full detail, including how the bundled dictionary is licensed, is
in [docs/spell-check.md](../spell-check.md).
