if has('win32')
  set encoding=utf-8
  scriptencoding utf-8
  set runtimepath+=~/.vim
  set rop=type:directx
endif

" Force the use of py3
if exists('py2') && has('python')
elseif has('python3')
endif

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-vinegar'

Plug 'romainl/vim-qf'
Plug 'romainl/vim-qlist'
Plug 'tommcdo/vim-lion'

" Still thinking about this
Plug 'maralla/completor.vim'

Plug 'kana/vim-textobj-user'
Plug 'Julian/vim-textobj-brace' " j
Plug 'kana/vim-textobj-indent' " i
Plug 'sgur/vim-textobj-parameter' " ,

" Allow the distro to take care of it (if it can)
if !has('win32') && !executable('fzf')
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
endif

" Colorschemes
Plug 'morhetz/gruvbox'

call plug#end()

if !exists('g:loaded_fzf') && filereadable('/usr/share/vim/vimfiles/plugin/fzf.vim')
  source /usr/share/vim/vimfiles/plugin/fzf.vim
endif

if has("patch2111") || v:version >= 800
  unlet! skip_defaults_vim
  source $VIMRUNTIME/defaults.vim
else
  source ~/.vim/defaults.vim
endif

runtime macros/matchit.vim

if has('gui_running')
  if has('win32')
    set guifont=DejaVu_Sans_Mono:h10.5
  else
    set guifont=DejaVu\ Sans\ Mono\ 10.5
  endif

  set guioptions=egc "-TmLr
  set lines=45 columns=125
else
  if has('mouse_sgr')
    set ttymouse=sgr
  endif
endif

set laststatus=2
set showcmd
set switchbuf=useopen
set autoread

set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab

set ignorecase
set smartcase
set hlsearch

set sidescroll=1
set number

set wildignore+=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.vcxproj,*.vcproj,*.sdf
set wildignore+=*.dll,*.exe,*.pdb,*.lib,*.o
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags,cscope.*,TAGS
set wildignore+=*.tar.*
set wildignorecase
set wildmode=full
set wildcharm=<C-z>

set path=.,**

set statusline=%*%<\ %n\ %r%m%f
set statusline+=\ %l:%c
set statusline+=%=%w%q%y[%{&ff}][%{&enc}]

" Fix slow o/O insert
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Turn folding off for real, hopefully
set foldmethod=manual
set nofoldenable

set splitbelow
set splitright

set exrc
set hidden
set complete-=i
set completeopt=longest,menuone
set formatoptions=tcrqnl1j

set history=1000
set tabpagemax=50

set noswapfile
set nobackup

" set path=.,**
set sessionoptions+=resize
if exists('+breakindent')
  set showbreak=...\ 
endif

"set listchars=tab:»\ ,extends:.,precedes:.,nbsp:·,trail:·
let &listchars = "tab:\u00bb\u00b7,trail:\u2022,extends:\u00bb,precedes:\u00ab,nbsp:\u00ba"
"let &fillchars = "vert:\u2591,fold:\u00b7"
set list

set viminfo='33,<200,s100,h
set virtualedit=block
set ttyfast

if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

" undo options
set undodir=~/.vim/cache/undo/
set undofile

if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

if has('gui_running')
  let g:solarized_italic=0
  let g:solarized_visibility="normal"
  let g:solarized_hitrail=1
  set background=light
  colorscheme gruvbox
else
  if has('termguicolors')
    if $TERM ==# 'tmux-256color' || $TERM ==# 'xterm-256color'
      set t_8f=[38;2;%lu;%lu;%lum " Needed in tmux
      set t_8b=[48;2;%lu;%lu;%lum " Ditto
      set termguicolors
    endif

    " set cursor shapes by mode in tmux
    " let &t_SI = "\<Esc>[6 q"
    " let &t_SR = "\<Esc>[4 q"
    " let &t_EI = "\<Esc>[2 q"
  endif

  set background=dark
  colorscheme gruvbox

  set colorcolumn=+1
  set cursorline
endif

if executable("rg")
    set grepprg=rg\ --vimgrep
    set grepformat=%f:%l:%c:%m,%f:%l:%m
elseif executable("ag")
    set grepprg=ag\ --ignore\ tags\ --ignore\ TAGS\ --vimgrep
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

augroup VIMRC
  autocmd!

  " This is annoying
  autocmd VimEnter,GUIEnter * set visualbell t_vb=

  " Save options
  autocmd BufLeave * let b:winview = winsaveview()
  autocmd BufEnter * if exists('b:winview') | call winrestview(b:winview) | endif

  " No cursorline in inactive buffers
  autocmd VimEnter,WinEnter,BufWinEnter,FocusGained,CmdwinEnter * setlocal cursorline
  autocmd WinLeave,FocusLost,CmdwinLeave * setlocal nocursorline
augroup END

xnoremap <leader>y "+y
nnoremap Y y$

nnoremap <silent> <leader>0 :call clearmatches()<cr>
nnoremap <silent> <leader>1 :call highlight#word(1)<cr>
nnoremap <silent> <leader>2 :call highlight#word(2)<cr>
nnoremap <silent> <leader>3 :call highlight#word(3)<cr>
nnoremap <silent> <leader>4 :call highlight#word(4)<cr>
nnoremap <silent> <leader>5 :call highlight#word(5)<cr>
nnoremap <silent> <leader>6 :call highlight#word(6)<cr>

" Don't move the cursor after * and #
nnoremap <silent> * :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>
nnoremap <silent> # :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>:let v:searchforward=0<CR>

" -romainl-
nnoremap <space>f :find *
nnoremap <space>s :sfind *
nnoremap <space>v :vert sfind *
nnoremap <space>F :find <C-R>=fnameescape(expand('%:p:h')).'/**/*'<CR>
nnoremap <space>S :sfind <C-R>=fnameescape(expand('%:p:h')).'/**/*'<CR>
nnoremap <space>V :vert sfind <C-R>=fnameescape(expand('%:p:h')).'/**/*'<CR>

command! -nargs=+ -complete=file_in_path -bar Grep  silent! grep! <args> | redraw! | copen
command! -nargs=+ -complete=file_in_path -bar LGrep silent! lgrep! <args> | redraw! | copen
nnoremap <space>g :Grep <C-r><C-w>

xnoremap <silent> <space>g :<C-u>let cmd = "Grep " . visual#GetSelection() <bar>
                        \ call histadd("cmd", cmd) <bar>
                        \ execute cmd<CR>

nnoremap <silent> <C-l> :set hlsearch!<CR>
nnoremap <BS> <C-^>

" Improved n/N - center line after page scroll
" Not very usefull for now
" function! s:nice_next(cmd)
"     let topline  = line('w0')
"     let v:errmsg = ""
"     execute "silent! normal! " . a:cmd
"     if v:errmsg =~ 'E38[45]:.*'
"         echohl Error | unsilent echom v:errmsg | echohl None
"         let v:errmsg = ""
"         return
"     endif
"     if topline != line('w0')
"         normal! zz
"     endif
" endfun

" nnoremap <silent> n :call <SID>nice_next('n')<cr>
" nnoremap <silent> N :call <SID>nice_next('N')<cr>

augroup FUGITIVE
  autocmd!

  autocmd BufReadPost fugitive://* set bufhidden=delete

  autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif
augroup END

if has('win32')
  let g:completor_clang_binary = 'd:/tools/prog/x64/LLVM/bin/clang.exe'
else
  let g:completor_clang_binary = '/usr/bin/clang'
endif

let g:netrw_home = '~/.vim/cache/'
