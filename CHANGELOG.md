# Changelog

## 2026-07-06

- Added a storage-location classifier that recognizes local paths, local `file:` URIs, and remote URI-backed document locations.
- Documented that remote storage is discoverable but not yet openable or saveable through the current local file IO.
- Blocked remote URI open/save-as workflow submissions before filesystem path parsing, keeping the modal open with a clear unsupported-storage status.
- Added parent breadcrumbs to nested settings search result rows in the command runner so preset settings are discoverable from search.
- Added release-cycle notes so the latest desktop release includes a human-readable changelog section alongside recent commits and downloadable assets.
