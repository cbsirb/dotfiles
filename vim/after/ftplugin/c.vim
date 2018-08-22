setlocal commentstring=//\ %s

setlocal cindent
setlocal cinoptions=t0,:0,(0

setlocal shiftwidth=4
setlocal softtabstop=4
setlocal expandtab

setlocal tw=110

inoremap <buffer> <C-l> ->
noremap <buffer> <space><space> :Silent exe "make " . expand("%:r") . ".o " \| redraw!<CR>
