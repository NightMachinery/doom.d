-- pandoc Lua filter: clean up HTML mail before it is written as Org.
--
-- Used by `night/h-html-to-org' in autoload/night-email.el:
--   pandoc -f html -t org --wrap=none --lua-filter=email/html-to-org.lua
-- See docs/email.md.

-- Images: keep the alt text. Tracking pixels have none and vanish.
function Image(el)
  local alt = pandoc.utils.stringify(el.caption)
  if alt == '' then return {} end
  return pandoc.Str('[image: ' .. alt .. ']')
end

-- A link left with no text (it wrapped an image without alt text) has
-- nothing to click, and Org cannot write an empty description. pandoc walks
-- bottom-up, so Image above has already run on the link's content.
function Link(el)
  if pandoc.utils.stringify(el.content):match('^%s*$') then return {} end
  return el
end

-- Org shows `\\' literally; a real newline reads better.
function LineBreak() return pandoc.RawInline('org', '\n') end

-- Styling wrappers and raw HTML carry nothing worth reading.
function Span(el) return el.content end
function Div(el) return el.content end
function Figure(el) return el.content end
function RawInline(el) if el.format ~= 'org' then return {} end end
function RawBlock(el) if el.format ~= 'org' then return {} end end

-- Ids and styles would become :PROPERTIES: drawers.
function Header(el) el.attr = pandoc.Attr(); return el end
function CodeBlock(el) el.attr = pandoc.Attr(); return el end

local function rows(tbl)
  local out = {}
  for _, r in ipairs(tbl.head.rows) do out[#out + 1] = r end
  for _, b in ipairs(tbl.bodies) do
    for _, r in ipairs(b.head) do out[#out + 1] = r end
    for _, r in ipairs(b.body) do out[#out + 1] = r end
  end
  for _, r in ipairs(tbl.foot.rows) do out[#out + 1] = r end
  return out
end

local function simple(cell)
  local c = cell.contents
  return #c == 0 or (#c == 1 and (c[1].t == 'Plain' or c[1].t == 'Para'))
end

-- HTML mail lays out its page with nested tables. A data table has at least
-- two rows and two columns of one-paragraph cells; any other table is layout,
-- and is replaced by its cells' contents in reading order. pandoc walks
-- bottom-up, so inner tables are already flat when their parent is judged.
function Table(tbl)
  local rs = rows(tbl)
  local data = #rs >= 2 and #tbl.colspecs >= 2
  for _, r in ipairs(rs) do
    for _, cell in ipairs(r.cells) do
      if not simple(cell) or cell.row_span > 1 or cell.col_span > 1 then
        data = false
      end
    end
  end
  if data then return tbl end
  local out = {}
  for _, r in ipairs(rs) do
    for _, cell in ipairs(r.cells) do
      for _, b in ipairs(cell.contents) do
        if b.t == 'Plain' then b = pandoc.Para(b.content) end
        out[#out + 1] = b
      end
    end
  end
  return out
end
