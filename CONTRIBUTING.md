# Contributing

Read [DEVELOPMENT.md](DEVELOPMENT.md) for the build, the checks and the coding standards.

## Licensing

Serenity is licensed under the GNU General Public License, version 3 or (at your option) any later version
(`GPL-3.0-or-later`). Contributions are accepted on the same terms: by submitting a change you agree that it is
licensed under `GPL-3.0-or-later` (inbound = outbound), and that you have the right to submit it. There is no
contributor licence agreement and no copyright assignment; you keep the copyright in your work.

Source files do not carry per-file licence headers. The `LICENSE` file and the `licenses` setting in `build.sbt` apply to
the whole repository.

### Adding a dependency or a bundled asset

A runtime dependency must be compatible with GPL-3.0-or-later: permissive licences (Apache-2.0, MIT, BSD, Unicode),
the LGPL, and the MPL-2.0 are; EPL-only, CDDL-only and proprietary licences are not. Then:

1. Add a row to `third-party/modules.tsv` (or `third-party/assets.tsv` for a font, icon or other asset), with its
   licence texts under `third-party/texts/`.
2. Run `sbt generateThirdPartyNotices` and commit the updated `THIRD-PARTY-NOTICES.md`.

CI runs `sbt checkThirdPartyNotices` and fails when a runtime dependency has no row or the notices are stale.
