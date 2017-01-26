function! s:showInPreview(list)
  if len(a:list) == 0
    return
  endif

  if len(a:list) > 1
    let l:ulist = filter(copy(a:list), 'index(a:list, v:val, v:key+1)==-1')
  endif

  let l:pbuf = bufnr('*tag-preview*', 1)

  let l:curft = &filetype
  exec "sbuffer " . l:pbuf
  exec "%d"
  exec "resize " . (len(l:ulist) * 2)
  "exec ":set filetype=" . l:curft

  exec ":setlocal filetype=" . l:curft
  setlocal pvw
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nonumber
  setlocal showbreak=

  call append(0, l:ulist)
  normal gg
  exec "wincmd p"
endfunction


function! tags#PreviewTag()
  execute "normal! mt"

  let l:start_line = line(".")
  let l:stop_line = line(".") - 7
  if l:stop_line < 0
    let l:stop_line = 0
  endif

  " if search("(\\|;", "b", l:stop_line) == 0
  if search(";\\|}\\|=", "b", l:stop_line) == 0
    return
  endif

  if search("(", '', l:start_line) == 0
    return
  endif

  if getline(".")[col(".") - 1] == "("
    execute "normal! b"

    let l:curword = expand('<cword>')
    let l:signatures = []

    if index(split(&cinwords, ","), l:curword) >= 0
      return
    endif

    let l:tlist = map(split(&tags, ','), 'fnamemodify(v:val, ":p")')
    let l:tags_files = filter(copy(l:tlist), 'index(l:tlist, v:val, v:key+1)==-1')

    for tf in l:tags_files
      let l:tpath = findfile(tf)

      if len(l:tpath) == 0
        continue
      endif

      " for line in systemlist('grep -F -w ' . l:curword . ' ' . l:tpath)
      for line in systemlist('grep -P -w "^' . l:curword . '" ' . l:tpath)
        let l:sigoff = match(line, 'signature:')

        if l:sigoff != -1
          let l:sigend = match(line[l:sigoff:], '\t')
          if l:sigend != -1
            let l:sigend += l:sigoff - 1
            let l:signatures = add(l:signatures, line[l:sigoff + 10 : l:sigend])
          else
            let l:signatures = add(l:signatures, line[l:sigoff + 10 :])
          endif
        endif
      endfor
    endfor

    call s:showInPreview(l:signatures)
  endif

  execute "normal! `t"
endfunction
