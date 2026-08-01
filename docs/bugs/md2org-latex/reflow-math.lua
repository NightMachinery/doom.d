function Math(el)
  el.text = el.text:gsub("%s*\n%s*", " ")
  return el
end
