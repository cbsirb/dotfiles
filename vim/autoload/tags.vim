function! s:showInPreview(list, tagname) abort
  if len(a:list) == 0
    return
  endif

  let pbuf = bufnr('Tag Preview', 1)
  echom "pbuf: " . string(pbuf)

  let curft = &filetype

  " Switch to preview buffer and clear it
  exec "sbuffer " . pbuf
  setlocal noreadonly
  exec "%d"

  " make the tag preview filetype the same as the source file
  exec ":setlocal filetype=" . curft
  setlocal pvw
  setlocal buftype=nofile
  setlocal bufhidden=wipe
  setlocal noswapfile
  setlocal nonumber
  setlocal showbreak=
  setlocal nobuflisted

  if len(a:list) > 1
    let ulist = filter(copy(a:list), 'index(a:list, v:val, v:key+1)==-1')
  else
    let ulist = a:list
  endif

  call append(0, ulist)

  setlocal readonly

  if exists("g:loaded_ale") && g:loaded_ale
    exec "ALEDisable"
  endif

  exec "resize " . (len(ulist))
  normal gg
  exec "NoMatchParen"
  exec "wincmd p"
endfunction


function! tags#PreviewTag() abort
  let window = winsaveview()

  let start_line = line(".")
  let stop_line = line(".") - 10
  if stop_line < 0
    let stop_line = 0
  endif

  " Search backwards for one of: ;{}=
  " We assume that that's were the current call ends
  " Should check for comments, but that's beyond my use case
  if search(";\\|}\\|{\\|=", "b", stop_line) == 0
    call winrestview(window)
    return
  endif

  " Search forward for '(' - where the function call starts
  if search("(", '', start_line) == 0
    call winrestview(window)
    return
  endif

  if getline(".")[col(".") - 1] != "("
    call winrestview(window)
    return
  endif

  execute "normal! b"

  let curword = expand('<cword>')
  let signatures = []

  if index(split(&cinwords, ","), curword) >= 0
    call winrestview(window)
    return
  endif

  let tagdict = taglist('^' . curword . '$')

  for line in tagdict
    if !has_key(line, 'signature')
      continue
    endif

    let signatures = add(signatures, get(line, 'signature'))
  endfor

  call s:showInPreview(signatures, curword)

  call winrestview(window)
endfunction
