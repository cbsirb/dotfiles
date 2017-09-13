" A very dumb search for alternate file base on extension only
" Maybe later I will support path-based (for python tests, etc.)
" eg: { 'py': ['tests/{}.py'] }

let s:alternate_extensions = {
  \ 'c': ['h'],
  \ 'ch': ['c', 'cpp'],
  \ 'cpp': ['h', 'hpp', 'hxx', 'h++']
  \}

let g:alt_ext = {
  \ 'c': ['h'],
  \ 'ch': ['c', 'cpp'],
  \ 'cpp': ['h', 'hpp', 'hxx', 'h++']
  \}

function! alternate#alternate_file() abort
  if !has_key(s:alternate_extensions, &filetype)
    return
  endif

  let current_file = expand('%:t:r')
  let alternate_files = []

  for ext in s:alternate_extensions[&filetype]
    let alternate_file = findfile(current_file . '.' . ext)
    if len(alternate_file)
      call add(alternate_files, alternate_file)
    endif
  endfor

  if len(alternate_files) < 1
    return
  elseif len(alternate_files) == 1
    exe 'edit ' . alternate_files[0]
    return
  endif

  let index = 1
  for file in alternate_files
    echom "[" . index . "] " . file
    let index += 1
  endfor

  let index = input("Which file to open?")

  if index < 1 || index > len(alternate_files)
    return
  endif

  exe 'edit ' . alternate_files[index - 1]
endfunction
