setlocal commentstring=//\ %s
" setlocal foldmethod=syntax

setlocal cindent
setlocal cinoptions=t0,:0,(0

inoremap <C-l> ->

" let b:switch_custom_definitions =
"   \ [
"   \   g:switch_builtins.cpp_pointer,
"   \   {
"   \     '\CTRUE':  'FALSE',
"   \     '\CFALSE': 'TRUE',
"   \     '\(\k\+\)Release': '\1Acquire',
"   \     '\(\k\+\)Acquire': '\1Release'
"   \   },
"   \ ]
