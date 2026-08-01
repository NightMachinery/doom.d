--: Converts display math to explicit `\begin{equation*}` environments,
--: preserving interior newlines. Org parses these line-wise as
--: latex-environment elements, so they are immune to the lone-operator
--: plain-list bug that breaks multi-line `\[...\]` fragments.
--: Inline math (and display math mixed into a text paragraph) is instead
--: reflowed onto one line, where fragments are safe.

local function reflow(s)
  return (s:gsub('%s*\n%s*', ' '))
end

function Math(el)
  if el.mathtype == 'InlineMath' then
    el.text = reflow(el.text)
    return el
  end
end

local function display_para(para)
  --: If PARA is display math standing alone, return an explicit
  --: environment block; if it mixes math and text, reflow the math.
  local display = nil
  for _, item in ipairs(para.content) do
    if item.t == 'Math' and item.mathtype == 'DisplayMath' and not display then
      display = item
    elseif item.t ~= 'Space' and item.t ~= 'SoftBreak' then
      return pandoc.walk_block(para, {
        Math = function(m)
          if m.mathtype == 'DisplayMath' then
            m.text = reflow(m.text)
            return m
          end
        end,
      })
    end
  end
  if display then
    local text = display.text:gsub('^%s*', ''):gsub('%s*$', '')
    return pandoc.RawBlock('org',
      '\\begin{equation*}\n' .. text .. '\n\\end{equation*}')
  end
end

Para = display_para
Plain = display_para
