" Force the .h files to headers (by default it detects it as CPP)

augroup CHEADER
  autocmd!

  au BufNewFile,BufRead *.h set filetype=ch
  au BufNewFile,BufRead *.hpp set filetype=ch
  au BufNewFile,BufRead *.h++ set filetype=ch
  au BufNewFile,BufRead *.hxx set filetype=ch
augroup END
