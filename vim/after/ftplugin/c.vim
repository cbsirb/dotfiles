setlocal commentstring=//\ %s

setlocal cindent
setlocal cinoptions=t0,:0,(0

setlocal shiftwidth=4
setlocal softtabstop=4
setlocal expandtab

setlocal tw=110

inoremap <buffer> <C-l> ->
noremap <buffer> <space><space> :Silent exe "make " . expand("%:r") . ".o " \| redraw!<CR>

let b:sandwich_recipes = deepcopy(g:sandwich#default_recipes)
let b:sandwich_recipes += [
      \   {
      \     'buns'        : ['{', '}'],
      \     'motionwise'  : ['line'],
      \     'kind'        : ['add'],
      \     'linewise'    : 1,
      \     'command'     : ["'[+1,']-1normal! >>"],
      \   },
      \   {
      \     'buns'        : ['{', '}'],
      \     'motionwise'  : ['line'],
      \     'kind'        : ['delete'],
      \     'linewise'    : 1,
      \     'command'     : ["'[,']normal! <<"],
      \   }
      \ ]
