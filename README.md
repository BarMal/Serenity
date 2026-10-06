# Serenity

A text editor for writing, built with Scala 3 and Cats Effect. See [DEVELOPMENT.md](DEVELOPMENT.md) to build and run it
and [CONTRIBUTING.md](CONTRIBUTING.md) before sending a change.

## Licence

Serenity is free software, released under the [GNU General Public License, version 3 or (at your option) any later
version](LICENSE) (SPDX: `GPL-3.0-or-later`).

It bundles third-party libraries and fonts under their own licences, listed with their copyright notices and full texts
in [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md). That file is generated from the build (`sbt generateThirdPartyNotices`)
and checked in CI. The same text ships inside the application: run **Show Licence and Notices** from the command
palette. The bundled Monaspace font is under the SIL Open Font License; its text is at
[src/main/resources/fonts/OFL.txt](src/main/resources/fonts/OFL.txt).
