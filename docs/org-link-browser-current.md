# `night/org-link-browser-current`

`night/org-link-browser-current` inserts the current browser link as an Org link.
When the current URL is a Google search URL, it auto-converts the URL into the
local Org `search:` link form:

```org
[[search:تور یک روزه قطار شمال]]
```

Google search query components are decoded as UTF-8 after translating `+` to
spaces. This is necessary for non-ASCII queries such as Persian text, because
`url-unhex-string` returns raw decoded bytes rather than a multibyte Emacs
string.
