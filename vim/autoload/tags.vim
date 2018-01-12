function! s:showInPreview(list, tagname) abort
  if len(a:list) == 0
    return
  endif

  let last_buf = bufwinnr(bufname('%'))

  " Switch to preview buffer and clear it
  let pwin = bufwinnr('Tag Preview')
  if pwin != -1
    exec pwin . "wincmd w"
  else
    exec "sbuffer " . bufnr('Tag Preview', 1)
  endif

  setlocal noreadonly
  exec "%d"

  " make the tag preview filetype the same as the source file
  setlocal previewwindow
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

  exec "resize " . (len(ulist))
  normal gg
  exec "NoMatchParen"
  exec last_buf . "wincmd w"
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
