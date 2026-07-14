# Windows Process Model

Serenity's Windows app image is built with `jpackage` using one main launcher,
`Serenity.exe`, and one main class, `Main`.[1] Oracle documents that a Windows
`jpackage` app image contains an application launcher and a bundled runtime.[2]

The Windows CI check observed and now asserts two Serenity processes while the
application is open:

1. `Serenity.exe` is the native `jpackage` launcher.
2. A child `Serenity.exe` hosts the application JVM.

This is one application with one JVM, not a duplicate editor instance. The
generated runtime does not expose a separate `java.exe` or `javaw.exe` process.
After normal window close, the launcher and child must both be gone.

This expected topology was verified by [PR #709](https://github.com/BarMal/Serenity/pull/709).

The Windows package-check and publish workflows launch the generated app image,
record process names, command lines, parentage, CPU time, and working-set memory,
then assert the launcher/child topology. The child receives a normal window-close
request, and the check waits for both processes to exit before verifying that no
process remains.[1] Forced termination is used only to clean up a failed check;
it never counts as a successful shutdown validation. A launcher process or child
that survives normal shutdown is a packaging or launcher defect.

## References

1. `.github/workflows/desktop-release.yml`; `.github/workflows/desktop-publish.yml`
2. [Oracle jpackage packaging overview](https://docs.oracle.com/en/java/javase/26/jpackage/packaging-overview.html)
