package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.serenity.command.{Command, CommandRunner}
import com.serenity.state.models.CursorPosition
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.theme.Theme

/** Renderer for the command runner overlay */
object CommandRunnerRenderer:

  /** Render the command runner overlay centered horizontally and positioned beneath the cursor */
  def render(
    graphics: TextGraphics,
    commandRunner: CommandRunner,
    theme: Theme,
    terminalSize: TerminalSize,
    cursorPosition: CursorPosition
  ): Unit =
    if commandRunner.isActive then
      val overlayWidth  = math.min(60, terminalSize.width - 4) // Leave margins
      val overlayHeight = math.min(8, terminalSize.height - 4) // Input + up to 5 commands + borders

      // Center horizontally on screen regardless of cursor position
      val overlayX = (terminalSize.width - overlayWidth) / 2

      // Position overlay directly beneath cursor line
      // cursorPosition now contains actual screen coordinates from the main renderer
      val cursorScreenLine = cursorPosition.line
      val preferredY       = cursorScreenLine + 1 // One line below cursor

      // Check if overlay fits below cursor, otherwise position above
      val overlayY =
        if preferredY + overlayHeight <= terminalSize.height then preferredY
        else math.max(0, cursorScreenLine - overlayHeight)

      renderOverlay(graphics, commandRunner, theme, overlayX, overlayY, overlayWidth, overlayHeight)

  def renderInRect(
    graphics: TextGraphics,
    commandRunner: CommandRunner,
    theme: Theme,
    rect: com.serenity.ui.layout.LayoutRect
  ): Unit =
    if commandRunner.isActive then
      renderOverlay(graphics, commandRunner, theme, rect.x, rect.y, rect.width, rect.height)

  private def renderOverlay(
    graphics: TextGraphics,
    commandRunner: CommandRunner,
    theme: Theme,
    x: Int,
    y: Int,
    width: Int,
    height: Int
  ): Unit =

    // Clear the overlay area with background color
    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)

    // Draw border
    drawBorder(graphics, x, y, width, height)

    // Draw search input at top
    val inputY = y + 1
    drawSearchInput(graphics, commandRunner, theme, x + 1, inputY, width - 2)

    // Draw command list
    val listY      = y + 3
    val listHeight = height - 4 // Account for border and input
    drawCommandList(graphics, commandRunner, theme, x + 1, listY, width - 2, listHeight)

  private def drawBorder(graphics: TextGraphics, x: Int, y: Int, width: Int, height: Int): Unit =
    // Top border
    graphics.putString(x, y, "┌" + "─" * (width - 2) + "┐")

    // Side borders
    for i <- 1 until height - 1 do
      graphics.putString(x, y + i, "│")
      graphics.putString(x + width - 1, y + i, "│")

    // Bottom border
    graphics.putString(x, y + height - 1, "└" + "─" * (width - 2) + "┘")

  private def drawSearchInput(
    graphics: TextGraphics,
    commandRunner: CommandRunner,
    theme: Theme,
    x: Int,
    y: Int,
    width: Int
  ): Unit =
    val prompt     = "> "
    val searchText = commandRunner.searchTerm
    val displayText =
      if searchText.length <= width - prompt.length then prompt + searchText
      else prompt + searchText.takeRight(width - prompt.length)

    // Clear the line
    graphics.putString(x, y, " " * width)

    // Draw the input with cursor
    graphics.putString(x, y, displayText)

    // Draw cursor at end of input
    val cursorX = x + displayText.length
    if cursorX < x + width then
      graphics.setForegroundColor(theme.backgroundColor)
      graphics.setBackgroundColor(theme.foregroundColor)
      graphics.putString(cursorX, y, " ")
      graphics.setForegroundColor(theme.foregroundColor)
      graphics.setBackgroundColor(theme.backgroundColor)

  private def drawCommandList(
    graphics: TextGraphics,
    commandRunner: CommandRunner,
    theme: Theme,
    x: Int,
    y: Int,
    width: Int,
    height: Int
  ): Unit =
    val visibleCommands = commandRunner.visibleCommands
    val selectedIndex   = commandRunner.selectedIndex
    val selectedCommand = commandRunner.selectedCommand

    // Draw separator line
    graphics.putString(x, y - 1, "─" * width)

    // Draw each command
    visibleCommands.zipWithIndex.foreach {
      case (command, index) =>
        if index < height then
          val lineY      = y + index
          val isSelected = selectedCommand.contains(command) // Check if this is the selected command

          drawCommandItem(graphics, command, theme, x, lineY, width, isSelected)
    }

    // Show scroll indicators if there are commands outside the viewport
    if commandRunner.hasMoreCommands && height > 0 then
      val totalCommands     = commandRunner.filteredCommands.length
      val visibleCount      = visibleCommands.length
      val selectedIndex     = commandRunner.selectedIndex
      val firstVisibleIndex = commandRunner.filteredCommands.indexOf(visibleCommands.head)
      val lastVisibleIndex  = firstVisibleIndex + visibleCount - 1

      graphics.setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)

      // Show "more above" indicator
      if firstVisibleIndex > 0 then graphics.putString(x, y - 2, s"↑ $firstVisibleIndex more above")

      // Show "more below" indicator
      if lastVisibleIndex < totalCommands - 1 then
        val remaining = totalCommands - lastVisibleIndex - 1
        graphics.putString(x, y + visibleCount, s"↓ $remaining more below")

      // Show current position
      graphics.putString(x + width - 15, y - 2, s"${selectedIndex + 1}/$totalCommands")

      graphics.setForegroundColor(theme.foregroundColor)

  private def drawCommandItem(
    graphics: TextGraphics,
    command: Command,
    theme: Theme,
    x: Int,
    y: Int,
    width: Int,
    isSelected: Boolean
  ): Unit =
    // Set colors for selection
    if isSelected then
      graphics.setForegroundColor(theme.backgroundColor)
      graphics.setBackgroundColor(theme.foregroundColor)
    else
      graphics.setForegroundColor(theme.foregroundColor)
      graphics.setBackgroundColor(theme.backgroundColor)

    // Clear the line
    graphics.putString(x, y, " " * width)

    // Format command name and description
    val nameWidth = math.min(15, width / 3)
    val descWidth = width - nameWidth - 3 // Space for padding

    val truncatedName =
      if command.name.length <= nameWidth then command.name.padTo(nameWidth, ' ')
      else command.name.take(nameWidth - 1) + "…"

    val truncatedDesc =
      if command.description.length <= descWidth then command.description
      else command.description.take(descWidth - 1) + "…"

    val displayText = s"$truncatedName   $truncatedDesc"
    graphics.putString(x, y, displayText.take(width))

    // Reset colors
    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)
