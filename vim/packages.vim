command! PackUpdate packadd minpac | source $MYVIMRC | redraw | call minpac#update()
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

if !exists('*minpac#init')
  finish
endif

call minpac#init()

call minpac#add('k-takata/minpac', {'type':'opt'})

call minpac#add('tpope/vim-apathy')
call minpac#add('tpope/vim-characterize')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-endwise')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-git')
call minpac#add('tpope/vim-ragtag')
call minpac#add('tpope/vim-repeat')
call minpac#add('tpope/vim-rsi')
call minpac#add('tpope/vim-sleuth')
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-vinegar')
call minpac#add('tpope/vim-surround')

call minpac#add('romainl/vim-qf')
call minpac#add('romainl/vim-qlist')
call minpac#add('romainl/vim-tinyMRU')

call minpac#add('tommcdo/vim-lion')

" call minpac#add('dense-analysis/ale')
" call minpac#add('Rip-Rip/clang_complete')

" When I start writing html again
" Plug 'rstacruz/sparkup'

call minpac#add('elzr/vim-json')
call minpac#add('Vimjas/vim-python-pep8-indent')

" call minpac#add('SirVer/ultisnips')
" call minpac#add('honza/vim-snippets')

call minpac#add('kana/vim-textobj-user')
call minpac#add('kana/vim-textobj-indent')    " i
call minpac#add('sgur/vim-textobj-parameter') " ,

call minpac#add('junegunn/fzf')

call minpac#add('prabirshrestha/async.vim')
call minpac#add('prabirshrestha/vim-lsp')
" call minpac#add('prabirshrestha/asyncomplete.vim')
" call minpac#add('prabirshrestha/asyncomplete-lsp.vim')
" call minpac#add('mattn/vim-lsp-settings')

" Colorschemes
call minpac#add('robertmeta/nofrils', {'type':'opt'})
call minpac#add('romainl/Apprentice', {'type':'opt'})
call minpac#add('romainl/flattened', {'type':'opt'})
call minpac#add('ajgrf/parchment', {'type':'opt'})
