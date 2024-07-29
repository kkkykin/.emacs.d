if FORMAT ~= "org" then
  function Str(elem)
    elem.text = elem.text:gsub("\u{200B}", "")
    return elem
  end
end
