# Sessions

A session is your open tabs, panes and layout. Serenity saves it when you close the application and when files change
(the defaults), and offers to resume it the next time you start.

## Resume

On the start page, press `Tab` to resume the last session. The page shows the file it will reopen. The same session can
be restored from the command palette with **Restore Session**.

## Commands

Run these from the command palette:

| Command | What it does |
| --- | --- |
| Save Session | Saves the current session now. |
| Restore Session | Restores the last saved session. |
| Clear Session | Clears the saved session. |
| Save Session As... | Saves the current session under a name, beside your other named sessions. |
| Open Session... | Opens one of your named sessions. |
| Rename Session... | Renames a named session. |
| Return to Start Page | Saves a snapshot and goes back to the start page. `Tab` resumes it. |

By default Serenity keeps the five most recent named sessions.

## Where they are

The saved session is `session-index.json` and the `sessions` folder in `~/.serenity`. To start without it once, use
`--reset-session`, which moves it aside rather than deleting it. See [safe mode and resets](safe-mode.md).
