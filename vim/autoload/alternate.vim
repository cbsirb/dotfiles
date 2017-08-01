" A very dumb search for alternate file base on extension only
" Maybe later I will support path-based (for python tests, etc.)
" eg: { 'py': ['tests/{}.py'] }

let s:alternate_extensions = {
  \ 'c': ['h'],
  \ 'ch': ['c', 'cpp'],
  \ 'cpp': ['h', 'hpp', 'hxx']
  \}

function! alternate#alternate_file()
  let l:current_file = expand('%:t:r')

  if !has_key(s:alternate_extensions, &filetype)
    return
  endif

  let l:alternate_files = []
  for i in s:alternate_extensions[&filetype]
    let l:alternate_file = findfile(l:current_file . '.' . i)
    if len(l:alternate_file)
      call add(l:alternate_files, l:alternate_file)
    endif
  endfor

  if len(l:alternate_files) < 1
    return
  elseif len(l:alternate_files) == 1
    exe 'edit ' . l:alternate_files[0]
    return
  endif

  let l:index = 1
  for file in l:alternate_files
    echom "[" . l:index . "] " . file
    let l:index += 1
  endfor

  let l:index = input("Which file to open?")

  if l:index < 1 || l:index > len(l:alternate_files)
    return
  endif

  exe 'edit ' . l:alternate_files[l:index - 1]
endfunction
