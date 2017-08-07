if has('win32')
  set encoding=utf-8
  set runtimepath+=~/.vim
  set rop=type:directx
endif

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'

Plug 'romainl/vim-qf'
Plug 'romainl/vim-qlist'
Plug 'romainl/vim-cool'
Plug 'romainl/vim-tinyMRU'

Plug 'tommcdo/vim-lion'

" Plug 'Rip-Rip/clang_complete'

" When I start writing html again
Plug 'rstacruz/sparkup'
Plug 'elzr/vim-json'
Plug 'Vimjas/vim-python-pep8-indent'

Plug 'w0rp/ale'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-indent' " i
Plug 'sgur/vim-textobj-parameter' " ,

if !has('win32')
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
endif

" Colorschemes
Plug 'morhetz/gruvbox'
Plug 'romainl/Apprentice'


call plug#end()

if has("patch2111") || v:version >= 800
  unlet! skip_defaults_vim
  source $VIMRUNTIME/defaults.vim
else
  source ~/.vim/defaults.vim
endif

runtime macros/matchit.vim

if !has('gui_running') && has('mouse_sgr')
    set ttymouse=sgr
endif

set lazyredraw

set laststatus=2
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
set wildignore+=*.pyc,*.class,*.sln,*.aps
set wildignore+=*.vcxproj,*.vcproj,*.sdf,*.filters,*.user
set wildignore+=*.dll,*.exe,*.pdb,*.lib,*.o,*.db
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
set wildignore+=.git/*,.hg/*,.svn/*
set wildignore+=__pycache__/*,*/__pycache__/*
set wildignore+=cscope.*,TAGS
set wildignore+=*.tar.*,*.zip,*.rar
set wildignorecase
set wildmode=longest:full,full
set wildcharm=<C-z>

" Will append whatever necessary per project (in the local .vimrc)
set path=.,**

set statusline=%*%<\ %n\ %r%m%f
set statusline+=\ %l:%c\ [%{ALEGetStatusLine()}]
set statusline+=%=%w%q%y[%{&ff}][%{&enc}]

set timeoutlen=1000
set ttimeoutlen=100

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

set sessionoptions+=resize
if exists('+breakindent')
  set showbreak=...\ 
endif

let &listchars = "tab:\u00bb\u00b7,trail:\u2022,extends:\u00bb,precedes:\u00ab,nbsp:\u00ba"
" let &fillchars = "vert:\u2591,fold:\u00b7"
set list

" set viminfo='100,<200,s100,h
set virtualedit=block
set ttyfast

" undo options
set undodir=~/.vim/cache/undo/
set undofile

if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set background=dark
colorscheme apprentice

set colorcolumn=+1
set cursorline

if !has('gui_running') && has('termguicolors')
  if $TERM =~# '-256color' && $TERM !~# 'rxvt'
    " Needed in tmux
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
  endif
endif

if executable("rg")
  set grepprg=rg\ --no-heading\ --vimgrep\ -g\ '!{.git,node_modules,vendor}/*'\ -g\ '!tags'\ -g\ '!TAGS'
  set grepformat=%f:%l:%c:%m,%f:%l:%m
elseif executable("ag")
  set grepprg=ag\ --ignore\ tags\ --ignore\ TAGS\ --vimgrep
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

augroup VIMRC
  autocmd!

  autocmd VimEnter,GUIEnter * set visualbell t_vb=

  autocmd BufLeave * if !&diff | let b:winview = winsaveview() | endif
  autocmd BufEnter * if exists('b:winview') && !&diff | call winrestview(b:winview) | unlet! b:winview | endif

  autocmd VimEnter,WinEnter,BufWinEnter,FocusGained,CmdwinEnter * setlocal cursorline
  autocmd WinLeave,FocusLost,CmdwinLeave * setlocal nocursorline
augroup END


command! -nargs=+ -complete=file_in_path -bar Grep  silent! grep! <args> | redraw! | copen
command! -nargs=+ -complete=file_in_path -bar LGrep silent! lgrep! <args> | redraw! | copen

command! -nargs=+ Silent execute 'silent <args>' | redraw!

command! SC vnew | setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile

command! -nargs=* -bang ReplaceSymbolInFunction silent call func#replace_symbol_in_function(<f-args>, '<bang>')

nnoremap Y y$

nnoremap <silent> <space>0 :call clearmatches()<cr>
nnoremap <silent> <space>1 :call highlight#word(1)<cr>
nnoremap <silent> <space>2 :call highlight#word(2)<cr>
nnoremap <silent> <space>3 :call highlight#word(3)<cr>
nnoremap <silent> <space>4 :call highlight#word(4)<cr>
nnoremap <silent> <space>5 :call highlight#word(5)<cr>
nnoremap <silent> <space>6 :call highlight#word(6)<cr>

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

nnoremap <space>j :tjump /

nnoremap <space>g :Grep -w '<C-r><C-w>' .<CR>
nnoremap <space>G :execute "Grep -w '<C-r><C-w>' " . expand('%:p:h')<cr>
nnoremap <space>c :Silent make -j4

xnoremap <silent> <space>g :<C-u>let cmd = "Grep " . visual#GetSelection() <bar>
                        \ call histadd("cmd", cmd) <bar>
                        \ execute cmd<CR>

xnoremap <silent> <space>G :<C-u>let cmd = "Grep " . visual#GetSelection() . " " . expand('%:p:h') <bar>
                        \ call histadd("cmd", cmd) <bar>
                        \ execute cmd<CR>

nnoremap <silent> <C-l> :set hlsearch!<CR>

nnoremap <BS> <C-^>

nnoremap <silent> <space>a :call alternate#alternate_file()<CR>

inoremap <silent> <F3> <C-o>:call tags#PreviewTag()<CR>
inoremap <silent> <F4> <C-o>:pclose<CR>

nnoremap <silent> <F3> :call tags#PreviewTag()<CR>
nnoremap <silent> <F4> :pclose<CR>

inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O

cnoremap <expr> <Tab>   getcmdtype() == "/" \|\| getcmdtype() == "?" ? "<CR>/<C-r>/" : "<C-z>"
cnoremap <expr> <S-Tab> getcmdtype() == "/" \|\| getcmdtype() == "?" ? "<CR>?<C-r>/" : "<S-Tab>"

nnoremap <Space>% :%s/\<<C-r>=expand('<cword>')<CR>\>/
nnoremap <Space>r :ReplaceSymbolInFunction <C-R><C-W> 

nmap <space>q <Plug>(qf_qf_toggle_stay)
nmap <space>l <Plug>(qf_loc_toggle_stay)

" Improved n/N - center line after page scroll
" Not very usefull for now
" function! s:nice_next(cmd)
"   let topline  = line('w0')
"   let v:errmsg = ""
"   execute "silent! normal! " . a:cmd
"   if v:errmsg =~ 'E38[45]:.*'
"     echohl Error | unsilent echom v:errmsg | echohl None
"     let v:errmsg = ""
"     return
"   endif
"   if topline != line('w0')
"     normal! zz
"   endif
" endfun

" nnoremap <silent> n :call <SID>nice_next('n')<cr>
" nnoremap <silent> N :call <SID>nice_next('N')<cr>

" Plugin configuration

augroup FUGITIVE
  autocmd!

  autocmd BufReadPost fugitive://* set bufhidden=delete

  autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif
augroup END

let g:netrw_home = '~/.vim/cache/'

let g:qf_mapping_ack_style = 1

let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_save = 1

let g:ale_linters = {
      \ 'cpp': ['gcc'],
      \ 'c': ['gcc'],
      \ 'python': ['pylint']
\}

let g:ale_statusline_format = ["\u2717 %d", "\u271a %d", "\u2713 ok"]
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

nmap <silent> <space>ek <Plug>(ale_previous_wrap)
nmap <silent> <space>ej <Plug>(ale_next_wrap)
nmap <silent> <space>ee <Plug>(ale_lint)

nnoremap <F5> :ME <C-z>

let g:clang_library_path = '/usr/lib64/libclang.so.4'
let g:clang_snippets = 0

let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir='~/.vim'

let g:vim_json_syntax_conceal = 1

let g:pymode_indent = 0
