# Music links in Org

Org local leader `lm` runs `night/org-link-hear-smart`. It picks the MPV
or Purple Telegram track that started or resumed most recently, including
paused tracks. With unavailable or tied timestamps, currently playing
Purple music takes precedence. Otherwise it prefers MPV.

Run `night/org-link-hear-mpv` or `night/org-link-hear-tlg` to choose a
specific player. `night/org-link-hear-get` also runs the smart command.
MPV produces ordinary `audiofile:` links. Telegram produces
`purpleaudio:` bookmarks for the local Purple profile.

Telegram requires `tdesktop-cli`, with local media information enabled to
create the link and media playback enabled to follow it. MPV timestamps
come from the `org_music_recency.lua` script in the MPV config.
