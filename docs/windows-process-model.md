# Windows Process Model

Serenity's Windows app image is built with `jpackage` using one main launcher,
`Serenity.exe`, and one main class, `Main`.[1] Oracle documents that a Windows
`jpackage` app image contains an application launcher and a bundled runtime.[2]

While Serenity is open, Windows Task Manager is therefore expected to show two
Serenity-related processes:

1. `Serenity.exe` is the native `jpackage` launcher.
2. `runtime\\bin\\java.exe` is its child process and hosts the JVM that runs
   Serenity.

This is one application with one JVM, not a duplicate editor instance. Both
processes must remain live while the window is open. After shutdown, neither
process may remain.

The Windows package-check and publish workflows launch the generated app image,
record process names, command lines, parentage, CPU time, and working-set memory,
then enforce the one-launcher/one-JVM relationship and post-exit cleanup.[1]
Any additional child process, a missing bundled `java.exe` child, or a process
that survives shutdown is a packaging or launcher defect.

## References

1. `.github/workflows/desktop-release.yml`; `.github/workflows/desktop-publish.yml`
2. [Oracle jpackage packaging overview](https://docs.oracle.com/en/java/javase/26/jpackage/packaging-overview.html)
