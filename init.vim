" vim: foldmethod=marker foldlevel=0 foldlevelstart=0
" TODO: checkbox handling
" {{{ Plugins
if has("nvim") | let path=$HOME."/.config/nvim/autoload/plug.vim"
else           | let path=$HOME."/.vim/autoload/plug.vim"
endif

if !filereadable(path)
    echo "Installing Plug..." | echo ""
    silent exec "!curl -fLo ".path." --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    let doinstall=1
endif

call plug#begin('~/.config/nvim/autoload/plugged')
Plug 'morhetz/gruvbox'
" {{{ Tweaks
" Plugins that improve core vim features
Plug 'tpope/vim-repeat'
"Plug 'powerman/AnsiEsc.vim'
Plug 'kana/vim-arpeggio'                " mappings {{{
Plug 'tpope/vim-unimpaired'
"Plug 'andrep/vimacs'                   " }}}
Plug 'kana/vim-niceblock'               " visual mode tweaks {{{
Plug 'zirrostig/vim-schlepp'
let g:Schlepp#allowSquishingLines = 1
let g:Schlepp#allowSquishingBlock = 1   " }}}
Plug 'vim-scripts/argtextobj.vim'       " text objects {{{
Plug 'machakann/vim-swap'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user'                " something depends on this, but I don't remember what. either way, probably useful
Plug 'vim-scripts/camelcasemotion'
Plug 'michaeljsmith/vim-indent-object'  " }}}
Plug 'junegunn/vim-peekaboo'            " surfacing info {{{
"Plug 'Yilin-Yang/vim-markbar'
Plug 'kshenoy/vim-signature'
"Plug 'vim-scripts/RelOps'              " }}}
" }}}
" {{{ Features
" IDE-like nonsense. Honestly, you probably shouldn't use any of these, except
" maybe fzf, mundo, and abolish
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-abolish'
Plug 'atondwal/vim-codepainter'
Plug 'dyng/ctrlsf.vim'
Plug 'simnalamburt/vim-mundo'
Plug 'godlygeek/tabular'
" replace with vim-easy-align?
" https://www.reddit.com/r/vim/comments/2lsr8d/vimeasyalign_the_most_ingenious_plugin_ive/
Plug 'majutsushi/tagbar'
let g:unicoder_cancel_normal = 1
Plug 'joom/latex-unicoder.vim'
Plug 'vim-scripts/loremipsum', { 'on' : 'Loremipsum' }
Plug 'junegunn/fzf'          , { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
"Plug 'metakirby5/codi.vim'   , { 'on' : 'Codi' }
Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
let g:ranger_replace_netrw = 1
    Plug 'rbgrouleff/bclose.vim' " for nvim
" }}}
" {{{ Languages
"Plug 'autozimu/LanguageClient-neovim', { 'do': './install.sh', 'branch' : 'next'}
"Plug 'chrisbra/csv.vim'
Plug 'rust-lang/rust.vim', { 'for' : 'rust' }
" {{{ Haskell
" TODO some sort of folding help
Plug 'dag/vim2hs' "Makes gf work on module names (broken?)
"Plug 'bitc/lushtags'       , { 'for' : 'haskell' }
"Plug 'eagletmt/ghcmod-vim' , { 'for' : 'haskell' }
" Plug 'bitc/vim-hdevtools' " Used with syntastic
"Plug 'davidhalter/jedi-vim'
"Plug 'parsonsmatt/intero-neovim'
" https://github.com/ucsd-progsys/liquid-types.vim
"Plug 'ndmitchell/ghcid'   , { 'rtp': 'plugins/nvim', 'on' : 'Ghcid' }
" }}}
"Plug 'idris-hackers/idris-vim'
Plug 'dense-analysis/ale'
Plug 'mattn/emmet-vim', { 'for' : 'html' } | let g:user_zen_mode='a'
" {{{ orgmode
Plug 'Detegr/vim-orgmode'
Plug 'dhruvasagar/vim-table-mode'
Plug 'vim-scripts/utl.vim'
Plug 'itchyny/calendar.vim' " mattn/calendar.vim?  Plug 'vim-scripts/calendar.vim'
Plug 'vim-scripts/SyntaxRange'
" }}}
" {{{ git
Plug 'jreybert/vimagit', {'on': 'Magit'}
Plug 'tpope/vim-fugitive' "for fzf
""bug that hides first line using either of these?!
Plug 'airblade/vim-gitgutter'
" Gets confused with the various undos
"Plug 'tweekmonster/gitbusy.vim'
" }}}
"Plug 'vim-scripts/LaTeX-Suite-aka-Vim-LaTeX', { 'for': 'tex' }
Plug 'lervag/vimtex'
Plug 'vim-pandoc/vim-pandoc'
"}}}
call plug#end() | if exists("doinstall") | PlugInstall | endif
" }}}

" {{{ Options
if !exists(g:syntax_on) | syntax enable | endif
set synmaxcol=800
set hidden
set showcmd
set mouse=a
"set number
set numberwidth=2
set ruler
"set so=7
set wildmenu wildmode=list:longest,full
set path=**,.,,
set foldlevelstart=99
set ve=all

set bg=dark
if &term=~'st' || &term=~'nvim' || &term=~'mlterm' || &term=~'tmux'
    set termguicolors
    set bg=dark
    let g:gruvbox_contrast_dark="hard"
    colors gruvbox
endif
set bg=dark
let g:gruvbox_contrast_dark="hard"
colors gruvbox
if has("nvim")
  hi ActiveWindow ctermbg=None ctermfg=None guibg=black
  hi InactiveWindow ctermbg=darkgray ctermfg=gray guibg=#282828
  set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
endif

set list listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set fillchars+=vert:│
set dictionary+=/usr/share/dict/words
set smarttab expandtab shiftwidth=2 tabstop=2
set undolevels=10000 undoreload=100000 undofile
if has("nvim")
  set undodir=~/.nvim/undo//
endif
" }}}
" {{{ Languages
" orgmode {{{
nmap  <localleader>cc
let g:org_agenda_files = ['~/org/asdf.org']
"         }}}
" {{{ latex
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat = 'pdf'
" }}}
"{{{ Language Client
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper']
    \ }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
map <Leader>ll :LanguageClientStop<CR>:LanguageClientStart<CR>
hi link ALEError GruvboxRedSign
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap
" Automatically start language servers.
let g:LanguageClient_autoStart = 1
"}}}
" {{{ Coq
" noremap <F5> :CoqLaunch<CR>
" au FileType coq call coquille#FNMapping()
" }}}}
" {{{ Haskell
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'i:instance:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type',
        \ 'i' : 'instance'
    \ },
    \ 'scope2kind' : {
        \ 'module'   : 'm',
        \ 'class'    : 'c',
        \ 'data'     : 'd',
        \ 'type'     : 't',
        \ 'instance' : 'i'
    \ }
\ }
"}}}
" }}}
" {{{ Mappings
tnore <S-Space> <Space>
nnoremap <CR> :
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>

omap i, <Plug>(swap-textobject-i)
xmap i, <Plug>(swap-textobject-i)
omap a, <Plug>(swap-textobject-a)
xmap a, <Plug>(swap-textobject-a)
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight

map Y y$
" Reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

inoremap <C-l> <Esc>:call unicoder#start(1)<CR>\
inore  :wqa
nore  :wqa

map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

command! R :Ranger

" {{{ Windows and Tabs
" let terminal resize scale the internal windows
"autocmd VimResized * :wincmd =

" Window Navigation with Alt
nnor <A-h> <C-w>h
nnor <A-j> <C-w>j
nnor <A-k> <C-w>k
nnor <A-l> <C-w>l
nnor <A-c> <C-w>c

nnor è <C-w>h
nnor ê <C-w>j
nnor ë <C-w>k
nnor ì <C-w>l
nnor ã <C-w>c

inor <A-h> <Esc><C-w>h
inor <A-j> <Esc><C-w>j
inor <A-k> <Esc><C-w>k
inor <A-l> <Esc><C-w>l
nnor <A-c> <C-w>c

inor è <Esc><C-w>h
inor ê <Esc><C-w>j
inor ë <Esc><C-w>k
inor ì <Esc><C-w>l
inor ã <Esc><C-w>c

if has("nvim")
  tnoremap <A-h> <C-\><C-n><C-w>h
  tnoremap <A-j> <C-\><C-n><C-w>j
  tnoremap <A-k> <C-\><C-n><C-w>k
  tnoremap <A-l> <C-\><C-n><C-w>l
  tnoremap <A-c> <C-\><C-n><C-w>c
  tnoremap è <C-\><C-n><C-w>h
  tnoremap ê <C-\><C-n><C-w>j
  tnoremap ë <C-\><C-n><C-w>k
  tnoremap ì <C-\><C-n><C-w>l
  tnoremap ã <C-\><C-n><C-w>c
endif

" Tab navigation with Ctrl-Shift
if has("nvim")
  tnoremap <A-S-h> <C-\><C-n>:tabprev<CR>
  tnoremap <A-S-l> <C-\><C-n>:tabnext<CR>
  tnoremap <A-S-n> <C-\><C-n>:tabnew term://zsh<CR>
  tnoremap  
  tnoremap È <C-\><C-n>:tabprev<CR>
  tnoremap Ì <C-\><C-n>:tabnext<CR>
endif

nnoremap <A-S-h> :tabprev<CR>
nnoremap <A-S-l> :tabnext<CR>
nnoremap <A-S-n> :tabe<CR>
nnoremap <A-n>   :tabnew term://zsh<CR>

nnoremap È :tabprev<CR>
nnoremap Ì :tabnext<CR>
nnoremap Î :tabe<CR>
nnoremap î :tabnew term://zsh<CR>

inoremap <A-S-h> :tabprev<CR>
inoremap <A-S-l> :tabnext<CR>
inoremap <A-S-n> :tabe<CR>
inoremap <A-n> :tabnew term://zsh<CR>

inoremap È :tabprev<CR>
inoremap Ì :tabnext<CR>
inoremap Î :tabe<CR>

if has("nvim")
  nnoremap <A-n>   :tabnew term://zsh<CR>
  nnoremap î :tabnew term://zsh<CR>
  inoremap <A-n> :tabnew term://zsh<CR>
  inoremap î :tabnew term://zsh<CR>
  tnoremap <A-n> :tabnew term://zsh<CR>
  tnoremap î :tabnew term://zsh<CR>
else
  nnoremap <A-n>   :tab terminal <CR>
  nnoremap î :tab terminal <CR>
  inoremap <A-n> :tab terminal <CR>
  inoremap î :tab terminal <CR>
  tnoremap <A-n> :tab terminal <CR>
  tnoremap î :tab terminal <CR>
endif

" }}}
call arpeggio#load() "{{{
Arpeggio imap jk <Esc>
Arpeggio map kl l
Arpeggio map hj h
Arpeggio map ui k
Arpeggio map nm j
Arpeggio map il gt
Arpeggio map hu gT
Arpeggio map ag :Ag<CR>
"Arpeggio imap ag <Esc>:Ag<CR>
" }}}
let mapleader = "\<Space>" " {{{
" {{{ Clipboard
vnoremap <Leader>y "+y
vnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P
" }}}

nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
nmap <silent> <leader>u :MundoToggle<CR>
nmap <leader>e :vsp $MYVIMRC<CR>
nmap <leader>f :Files<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>t :Tags<CR>
nmap <leader>a :Windows<CR>
nmap <leader>: :History:<CR>
nmap <leader>/ :History/<CR>
nmap <leader>h :History<CR>
nnoremap <Leader>z 1z=
inoremap <C-s> <C-g>u<Esc>[s1z=`]a

autocmd bufwritepost * if getfperm(expand("%:p")) =~ "x" | setl mp=% | endif
nnoremap <Leader>w :w<CR>
nnoremap <Leader>v :w<CR>
nnoremap <Leader>r :w<CR>:make<CR>
imap <F9> <Esc>:w<CR>:make<CR>

nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
nnoremap <leader><cr> :noh<cr>
nnoremap <leader>sp :set spell!<cr>
nnore <leader>x :!chmod +x %<cr>

" }}}
" }}}

" Source and quit
autocmd bufread $MYVIMRC map <buffer> <leader>e :so %\|wq<CR>
autocmd bufwritepost $MYVIMRC source $MYVIMRC
" Folds https://essais.co/better-folding-in-neovim/ {{{
set fillchars=fold:\
autocmd bufreadpre TODOs setlocal foldtext=CustomFoldText()
autocmd bufreadpre TODOs setlocal foldmethod=expr
autocmd bufreadpre TODOs setlocal foldexpr=GetPotionFold(v:lnum)
autocmd bufreadpre TODOs setlocal foldlevel=0

function! GetPotionFold(lnum)
  if getline(a:lnum) =~? '\v^\s*$'
      return '-1'
  endif

  if getline(a:lnum) =~ '^vim:'
    return '0'
  endif

  if getline(a:lnum) =~? '.*M PDT *\d*$'
      return '>1'
  endif
  if getline(a:lnum) =~? '.*M PST *\d*$'
      return '>1'
  endif

  let this_indent = IndentLevel(a:lnum)
  let next_indent = IndentLevel(NextNonBlankLine(a:lnum))

  if next_indent <= this_indent
    return this_indent
  elseif next_indent > this_indent
    return '>' . next_indent
  endif
endfunction

function! IndentLevel(lnum)
    return 1 + (indent(a:lnum) / &shiftwidth)
endfunction

function! NextNonBlankLine(lnum)
  let numlines = line('$')
  let current = a:lnum + 1

  while current <= numlines
      if getline(current) =~? '\v\S'
          return current
      endif

      let current += 1
  endwhile

  return -2
endfunction

function! CustomFoldText()
  " get first non-blank line
  let fs = v:foldstart

  while getline(fs) =~ '^\s*$' | let fs = nextnonblank(fs + 1)
  endwhile

  if fs > v:foldend
      let line = getline(v:foldstart)
  else
      let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
  endif

  let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
  let foldSize = 1 + v:foldend - v:foldstart
  let foldSizeStr = " " . foldSize . " lines "
  let foldLevelStr = repeat("+--", v:foldlevel)
  let expansionString = repeat(" ", w - strwidth(foldSizeStr.line.foldLevelStr))
  return line . expansionString . foldSizeStr . foldLevelStr
endfunction
" }}}

" {{{ Experimental
" {{{ Merlin -- OCaml
" Vim needs to be built with Python scripting support, and must be
" able to find Merlin's executable on PATH.
if executable('ocamlmerlin') " && has('python3')
  let s:ocamlmerlin = substitute(system('opam config var share'), '\n$', '', '''') . "/merlin"
  execute "set rtp+=".s:ocamlmerlin."/vim"
  execute "set rtp+=".s:ocamlmerlin."/vimbufsync"
  noremap <leader>d :MerlinDestruct
  noremap <leader>t :MerlinTypeOf
endif


if executable('ocp-indent') " && has('python3')
  autocmd FileType ocaml execute "set rtp+=" . substitute(system('opam config var share'), '\n$', '', '''') . "/ocp-indent/vim/indent/ocaml.vim"
endif
" }}}

"Plug 'vimwiki/vimwiki'
"
function! s:Transclude()
  let l:z = @z
  silent! g/@transclude{\zs.*\ze}/norm!gn"zyo@transcluded{o}k:r z
  let @z = l:z
endfunction

" augroup transcl
"   autocmd!
"   autocmd BufEnter *.wiki norm!:silent! lgrep! '\[\[=expand('%:t:r')\]\]' *.wiki
"   autocmd BufEnter *.wiki norm!:silent! lgrep! '=expand('%:t:r')' *.wiki
"   autocmd BufEnter *.wiki call s:LOpen()
"   autocmd BufWritePre *.wiki silent! g/@transcluded/norm!dV%
"   autocmd BufWritePost *.wiki call s:Transclude()
" augroup END
" 
" function! s:LTransclude()
"   let l:z = @z
"   silent! g/{{embed\s\+\[\[\zs.*\ze\]\]}}/norm!gn"zyo<!-- transcluded: start -->o<!-- transcluded: end -->k:r z.md
"   let @z = l:z
" endfunction
" 
" function! s:LOpen()
"   let l:winnr = winnr()
"   lopen
"   execute "norm!" . l:winnr . ""
" endfunction
" 
" function! s:ClosePrevLocationList()
"     if !exists('g:prev_win_id') || g:prev_win_id == 0
"         return
"     endif
" 
"     " Check if the previous window still exists
"     let prev_win_num = win_id2win(g:prev_win_id)
"     if prev_win_num == 0
"         return
"     endif
" 
"     " Get buffer info from previous window
"     let prev_buf = winbufnr(prev_win_num)
"     if prev_buf == -1
"         return
"     endif
" 
"     " Only proceed if previous buffer was markdown and current isn't quickfix
"     if &buftype !=# 'quickfix'
"         " Save current window ID
"         let current_win_id = win_getid()
"         
"         " Switch to previous window to close its location list
"         noautocmd call win_gotoid(g:prev_win_id)
"         silent! lclose
"         
"         " Return to original window
"         noautocmd call win_gotoid(current_win_id)
"     endif
" endfunction

function! s:LOpen()
  let l:winnr = winnr()
  lopen
  execute "norm!" . l:winnr . ""
endfunction

function! s:UpdateLocationList()
  " Only update if we're in a markdown buffer and not in quickfix
  if &buftype !=# 'quickfix'
    " Clear previous list first
    lexpr []
    " Search for both [[target]] and target references
    silent! lgrep! '\[\[%:t:r\]\]' *.md
    silent! lgrepadd! '%:t:r' *.md
    "silent! norm!:lgrepadd! '=expand('%:t:r')' *.md
  endif
endfunction

" augroup transcl
"   autocmd!
"   " autocmd BufEnter *.md silent! call s:LOpen()
"   autocmd BufEnter *.md call timer_start(50, { -> s:UpdateLocationList() })
"   autocmd WinLeave * let g:prev_win_id = win_getid()
"   autocmd WinEnter * call s:ClosePrevLocationList()
"   autocmd BufWritePre *.md silent! g/<!-- transcluded: start -->/,/<!-- transcluded: end -->/d
"   autocmd BufReadPost *.md call s:LTransclude()
" augroup END


augroup notion
  autocmd!
  autocmd BufNewFile *.n cd %:h
  autocmd BufNewFile *.n silent! r!fetch_xattrs.sh
  autocmd BufNewFile *.n silent! norm ggdd
  autocmd BufNewFile *.n set ft=csv
  autocmd BufNewFile *.n set ve=
  autocmd BufWritePre *.n set buftype=acwrite
  autocmd BufWriteCmd *.n silent! w !set_xattrs.sh /dev/stdin
  autocmd BufWriteCmd *.n silent! set nomod
  autocmd BufWritePost *.n set buftype=
augroup END


function! MyOnBattery()
  if filereadable("/sys/class/power_supply/ADP0/online")
    return readfile('/sys/class/power_supply/ADP0/online') == ['0']
  endif
  return readfile('/sys/class/power_supply/AC/online') == ['0']
endfunction

" if MyOnBattery() | call neomake#configure#automake('w')
" else             | call neomake#configure#automake('nw', 700)
" endif

"function! GitStatus()
"  let [a,m,r] = GitGutterGetHunkSummary()
"  if a > 0 || m > 0 || r > 0
"    return printf('+%d ~%d -%d', a, m, r)
"  endif
"  return ''
"endfunction
"set statusline+=%{GitStatus()}
" }}}
"
"
" https://chatgpt.com/c/67c89cb3-3d80-8010-87d6-37bc2de9c827
function! ClauseTextObject(inner) abort
  let l:punctuation = '[,;:.?!()]'

  " Save original position
  let l:orig_pos = getpos('.')

  " Find start of clause
  let l:start_pos = searchpos(l:punctuation, 'bnW')
  if l:start_pos == [0, 0]
    " If no punctuation found backward, start from buffer start
    call cursor(1, 1)
  else
    " Move one character after punctuation mark
    call cursor(l:start_pos[0], l:start_pos[1] + 1)
  endif

  " Skip whitespace after punctuation/start
  silent! normal! /\S<CR>
  let l:clause_start = getpos('.')

  " Restore original cursor
  call setpos('.', l:orig_pos)

  " Find end of clause
  let l:end_pos = searchpos(l:punctuation, 'nW')
  if l:end_pos == [0, 0]
    " If no punctuation found forward, end at buffer end
    call cursor(line('$'), col([line('$'), '$']))
  else
    " Move to one character before punctuation mark
    call cursor(l:end_pos[0], l:end_pos[1] - 1)
  endif

  " Skip whitespace backward from punctuation/end
  silent! normal! ?\S<CR>
  let l:clause_end = getpos('.')

  " Adjust positions if inner object selected
  if a:inner
    let start = [l:clause_start[1], l:clause_start[2]]
    let end = [l:clause_end[1], l:clause_end[2]]
  else
    " Include punctuation and whitespace around it
    if l:start_pos != [0,0]
      let start = [l:start_pos[0], l:start_pos[1]]
    else
      let start = [1, 1]
    endif
    if l:end_pos != [0,0]
      let end = [l:end_pos[0], l:end_pos[1]]
    else
      let end = [line('$'), col([line('$'), '$'])]
    endif
  endif

  " Set visual marks and select
  call setpos("'<", [0, start[0], start[1], 0])
  call setpos("'>", [0, end[0], end[1], 0])
  normal! gv
endfunction

function! MoveToClause(forward) abort
  let l:punctuation = '[,;:.?!()]'

  if a:forward
    " Move forward to punctuation, then forward again to start of next clause
    if search(l:punctuation, 'W') == 0
      echo "No next clause found"
      return
    endif
    silent! normal! /\S<CR>
  else
    " Move backward to punctuation, then backward again to start of previous clause
    if search(l:punctuation, 'bW') == 0
      echo "No previous clause found"
      return
    endif
    silent! normal! ?\S<CR>
  endif
endfunction

" Mapping
xnoremap ix :<C-u>call ClauseTextObject(1)<CR>
onoremap ix :<C-u>call ClauseTextObject(1)<CR>
xnoremap ax :<C-u>call ClauseTextObject(0)<CR>
onoremap ax :<C-u>call ClauseTextObject(0)<CR>

" Map the motions
nnoremap ]x :call MoveToClause(1)<CR>
nnoremap [x :call MoveToClause(0)<CR>
onoremap ]x :call MoveToClause(1)<CR>
onoremap [x :call MoveToClause(0)<CR>
xnoremap ]x :call MoveToClause(1)<CR>
xnoremap [x :call MoveToClause(0)<CR>


 " --- ALE Configuration for Python ---

" Set the linters you want to run for Python files.
" I recommend flake8 for general style/errors and mypy for type checking.
let g:ale_linters = {
\   'python': ['flake8', 'mypy'],
\}

" Set the tools you want to use to fix your code.
" black formats the code, isort sorts the imports.
" The order matters! Run black first, then isort.
let g:ale_fixers = {
\   'python': ['black', 'isort'],
\}

" Enable linting on text change and on save.
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_save = 1

" Enable fixing on save. This is a huge time-saver.
" let g:ale_fix_on_save = 1
