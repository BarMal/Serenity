# Windows Process Model

Serenity's Windows app image is built with `jpackage` using one main launcher,
`Serenity.exe`, and one main class, `Main`.[1] Oracle documents that a Windows
`jpackage` app image contains an application launcher and a bundled runtime.[2]

The app image guarantees one native launcher process:

1. `Serenity.exe` is the native `jpackage` launcher.

The generated runtime does not expose a stable Java executable name, so Serenity
does not assume that Task Manager will show a separate `java.exe` or `javaw.exe`
process. Any launcher descendants are recorded as part of the same application,
not counted as duplicate editor instances. After shutdown, the launcher and all
of its descendants must be gone.

The Windows package-check and publish workflows launch the generated app image,
record process names, command lines, parentage, CPU time, and working-set memory,
then enforce post-exit cleanup for the complete launcher process tree.[1]
A launcher process or descendant that survives shutdown is a packaging or
launcher defect.

## References

1. `.github/workflows/desktop-release.yml`; `.github/workflows/desktop-publish.yml`
2. [Oracle jpackage packaging overview](https://docs.oracle.com/en/java/javase/26/jpackage/packaging-overview.html)
