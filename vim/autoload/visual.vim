" return a representation of the selected text
" suitable for use as a search pattern
function! visual#GetSelection() abort
  let old_reg = getreg("v")
  normal! gv"vy
  let raw_search = getreg("v")
  call setreg("v", old_reg)
  return substitute(escape(raw_search, '\/.*$^~[]'), "\n", '\\n', "g")
endfunction
