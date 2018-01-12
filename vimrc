source ~/.vim/packages.vim

if !has('nvim')
  source $VIMRUNTIME/defaults.vim
  packadd matchit
endif

set lazyredraw
set laststatus=2
set switchbuf=useopen,usetab
set autoread
set report=0
set sidescroll=1
set hidden
set virtualedit=block
set shortmess+=c
set noswapfile
set nobackup
set nofoldenable

" Dangerous but usefull
set exrc

set autoindent
set smartindent
set shiftround
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab

set ignorecase
set smartcase
set hlsearch

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

set statusline=%<\ %f\ %y%w%r%m
set statusline+=%=
set statusline+=[%{ALEGetStatusLine()}]\ 
set statusline+=%l\/%-6L\ %3c\ 

set timeoutlen=1000
set ttimeoutlen=100
set ttyfast

set splitbelow
set splitright

set complete-=i
set completeopt=longest,menuone
set formatoptions=tcrqnl1j

set history=1000
set tabpagemax=50

set clipboard^=unnamedplus

set linebreak
set breakindent
set showbreak=...\ 
let &listchars = "tab:\u00bb\ ,trail:\u2022,extends:\u203a,precedes:\u2039,nbsp:\u00ba"
set list

set viminfo='100,<200,s100,h
set sessionoptions+=resize

set undodir=~/.vim/cache/undo/
set undofile

if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set background=light
set colorcolumn=+1
set cursorline

" Disabled 08.14.2017
" if !has('gui_running') && has('termguicolors')
"   if $TERM =~# '-256color' && $TERM !~# 'rxvt'
"     " Needed in tmux
"     let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"     let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
"     set termguicolors
"   endif
" endif

if executable("rg")
  set grepprg=rg\ --no-heading\ --vimgrep\ -g\ '!{.git,node_modules,vendor}/*'\ -g\ '!tags'\ -g\ '!TAGS'
  set grepformat=%f:%l:%c:%m,%f:%l:%m
elseif executable("ag")
  set grepprg=ag\ --ignore\ tags\ --ignore\ TAGS\ --vimgrep
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

augroup VIMRC
  autocmd!

  autocmd VimEnter * colorscheme solarized

  autocmd VimEnter,GUIEnter * set visualbell t_vb=

  autocmd BufLeave * if !&diff | let b:winview = winsaveview() | endif
  autocmd BufEnter * if exists('b:winview') && !&diff | call winrestview(b:winview) | unlet! b:winview | endif

  autocmd VimEnter,WinEnter,BufWinEnter,FocusGained,CmdwinEnter * setlocal cursorline
  autocmd WinLeave,FocusLost,CmdwinLeave * setlocal nocursorline

  if !has('nvim')
    autocmd CmdlineEnter /,\? :set hlsearch
  endif
augroup END

""""""""""""
" Commands "
""""""""""""
command! -nargs=+ -complete=file_in_path -bar Grep  silent! grep! <args> | redraw! | copen
command! -nargs=+ -complete=file_in_path -bar LGrep silent! lgrep! <args> | redraw! | copen

command! -nargs=+ Silent execute 'silent <args>' | redraw!

command! SC vnew | setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile

command! SS echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')

command! -nargs=* -bang ReplaceSymbolInFunction silent call func#replace_symbol_in_function(<f-args>, '<bang>')

""""""""""""
" Mappings "
""""""""""""
nnoremap Y y$

nnoremap \\d "_d
xnoremap \\d "_d

nnoremap \\p "_dP

nnoremap <expr> k      v:count == 0 ? 'gk' : 'k'
nnoremap <expr> j      v:count == 0 ? 'gj' : 'j'
nnoremap <expr> <Up>   v:count == 0 ? 'gk' : 'k'
nnoremap <expr> <Down> v:count == 0 ? 'gj' : 'j'

nnoremap <silent> <C-l> :set hlsearch!<CR>
nnoremap <BS> <C-^>

nnoremap <silent> <space>0 :call clearmatches()<cr>
nnoremap <silent> <space>1 :call highlight#word(1)<cr>
nnoremap <silent> <space>2 :call highlight#word(2)<cr>
nnoremap <silent> <space>3 :call highlight#word(3)<cr>
nnoremap <silent> <space>4 :call highlight#word(4)<cr>
nnoremap <silent> <space>5 :call highlight#word(5)<cr>
nnoremap <silent> <space>6 :call highlight#word(6)<cr>

nnoremap <silent> * :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>
nnoremap <silent> # :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>:let v:searchforward=0<CR>

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

" Poor man's pair insert
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O

inoremap <C-j> <C-x><C-p>

" Poor man's change/select inside/around characters
for char in [ '_', '.', ':', ';', '<bar>', '/', '<bslash>', '*', '=', '+', '%', '`', '#', '-' ]
  exe 'xnoremap i' . char . ' :<C-u>normal! T' . char . 'vt' . char . '<cr>'
  exe 'onoremap i' . char . ' :normal vi' . char . '<cr>'
  exe 'xnoremap a' . char . ' :<C-u>normal! F' . char . 'vf' . char . '<cr>'
  exe 'onoremap a' . char . ' :normal va' . char . '<cr>'
endfor

" Poor man's change/select inside/around line
xnoremap il g_o^
onoremap il :normal vil<CR>
xnoremap al $o0
onoremap al :normal val<CR>

" Move with <S-/Tab> forward and backward when searching
cnoremap <expr> <Tab>   getcmdtype() =~ '[\/?]' ? "<C-g>" : "<C-z>"
cnoremap <expr> <S-Tab> getcmdtype() =~ '[\/?]' ? "<C-t>" : "<S-Tab>"

cnoremap %% <C-r>=fnameescape(expand('%'))<cr>
cnoremap <C-r><C-l> <C-r>=getline('.')<CR>

" Poor man's alternate file
nnoremap <space>a :find <C-R>=fnameescape(expand('%:t:r')).'.'<CR><C-z><C-z>
nnoremap <space>A :vert sfind <C-R>=fnameescape(expand('%:t:r')).'.'<CR><C-z><C-z>

inoremap <silent> <F3> <C-o>:call tags#PreviewTag()<CR>
nnoremap <silent> <F3> :call tags#PreviewTag()<CR>

inoremap <silent> <F4> <C-o>:pclose<CR>
nnoremap <silent> <F4> :pclose<CR>

nnoremap <Space>% :%s/\<<C-r>=expand('<cword>')<CR>\>/
nnoremap <Space>r :ReplaceSymbolInFunction <C-R><C-W> 

"""""""""""""""""""
" Plugin mappings "
"""""""""""""""""""
" I'm not using this enough :sigh:
nnoremap <space>f :FZF<cr>
nnoremap <space>F :FZF <C-R>=fnameescape(expand('%:p:h'))<CR><CR>

nmap <space>q <Plug>qf_qf_stay_toggle
nmap <space>l <Plug>qf_loc_stay_toggle

" Keep unimpaired convention
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)

" Needed for vim-sandwich
nmap s <Nop>
xmap s <Nop>

""""""""""""""""""""""""""""""""""
" Built-in plugins configuration "
""""""""""""""""""""""""""""""""""
let g:netrw_home      = '~/.vim/cache/'
let g:netrw_altfile   = 1
let g:netrw_liststyle = 1
let g:netrw_sizestyle = 'H'
let g:netrw_winsize   = '30'

let g:ch_syntax_for_h = 1

"""""""""""""""""""""""""""""""""""
" 3rd party plugins configuration "
"""""""""""""""""""""""""""""""""""
augroup FUGITIVE
  autocmd!

  autocmd BufReadPost fugitive://* set bufhidden=delete

  autocmd User fugitive
        \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
        \   nnoremap <buffer> .. :edit %:h<CR> |
        \ endif
augroup END

let g:qf_mapping_ack_style = 1
let g:qf_statusline        = {}
let g:qf_statusline.before = '%<\ '
let g:qf_statusline.after  = '\ %f%=%l\/%-6L\ %3c\ '

let g:clang_library_path = '/usr/lib64/libclang.so'
let g:clang_snippets     = 0

let g:UltiSnipsEditSplit   = "vertical"
let g:UltiSnipsSnippetsDir = '~/.vim'

let g:vim_json_syntax_conceal = 1

let g:pymode_indent = 0

let g:ale_lint_on_text_changed = "never"
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_save         = 1

let g:ale_statusline_format = ["\u2717 %d", "\u271a %d", "\u2713 ok"]
let g:ale_echo_msg_format   = '[%linter%] %s [%severity%]'

let g:ale_python_pylint_executable = 'pylint'

let g:ale_linters = {
      \ 'cpp': ['gcc'],
      \ 'c': ['gcc'],
      \ 'python': ['pylint']
      \}

let g:fzf_layout = { 'down': '~20%' }
