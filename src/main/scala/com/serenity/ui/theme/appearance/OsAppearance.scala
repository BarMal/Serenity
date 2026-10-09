package com.serenity.ui.theme.appearance

enum OsAppearance:
  case Light, Dark, HighContrast, Unknown

/** What a finished query command printed. A command that could not be started at all is `None` where one is expected.
  */
final case class ProcessOutput(exitCode: Int, stdout: String):
  def succeeded: Boolean = exitCode == 0
  def text: String       = stdout.trim

/** Reads each OS's own answer to "light, dark or high contrast?".
  *
  * A query that exits non-zero is not an error where the OS simply omits the setting in its default state -- macOS has
  * no `AppleInterfaceStyle` key in light mode -- so those read as Light. `None`, a command that could not run, never
  * guesses: it is Unknown unless another query answered.
  */
object OsAppearanceParsers:

  private val MacContrastOn         = "1"
  private val WindowsHighContrastOn = 0x1
  private val AppsUseLightTheme     = """AppsUseLightTheme\s+REG_DWORD\s+0x([0-9a-fA-F]+)""".r.unanchored
  private val HighContrastFlags     = """Flags\s+REG_SZ\s+(\d+)""".r.unanchored

  def macOs(interfaceStyle: Option[ProcessOutput], increaseContrast: Option[ProcessOutput]): OsAppearance =
    if increaseContrast.exists(output => output.succeeded && output.text == MacContrastOn) then
      OsAppearance.HighContrast
    else
      interfaceStyle.fold(OsAppearance.Unknown) { output =>
        if output.succeeded && output.text.equalsIgnoreCase("dark") then OsAppearance.Dark else OsAppearance.Light
      }

  def windows(appsUseLightTheme: Option[ProcessOutput], highContrastFlags: Option[ProcessOutput]): OsAppearance =
    if highContrastFlags.exists(highContrastOn) then OsAppearance.HighContrast
    else
      appsUseLightTheme.fold(OsAppearance.Unknown) { output =>
        if !output.succeeded then OsAppearance.Light
        else
          output.stdout match
            case AppsUseLightTheme(hex) => if BigInt(hex, 16) == 0 then OsAppearance.Dark else OsAppearance.Light
            case _                      => OsAppearance.Unknown
      }

  private def highContrastOn(output: ProcessOutput): Boolean =
    output.succeeded && (output.stdout match
      case HighContrastFlags(flags) => flags.toLongOption.exists(value => (value & WindowsHighContrastOn) != 0)
      case _                        => false)

  def linux(colorScheme: Option[ProcessOutput], gtkTheme: Option[ProcessOutput]): OsAppearance =
    val scheme = colorScheme.filter(_.succeeded).map(unquoted)
    val gtk    = gtkTheme.filter(_.succeeded).map(unquoted)
    if gtk.exists(_.contains("highcontrast")) then OsAppearance.HighContrast
    else if scheme.contains("prefer-dark") then OsAppearance.Dark
    else if scheme.contains("prefer-light") then OsAppearance.Light
    else gtk.fold(OsAppearance.Unknown)(name => if name.contains("dark") then OsAppearance.Dark else OsAppearance.Light)

  private def unquoted(output: ProcessOutput): String =
    output.text.stripPrefix("'").stripSuffix("'").toLowerCase(java.util.Locale.ROOT)
