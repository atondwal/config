" vim: foldmethod=marker foldlevel=0
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
Plug 'kana/vim-arpeggio'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'vim-scripts/argtextobj.vim'
Plug 'kana/vim-niceblock'
Plug 'zirrostig/vim-schlepp'
Plug 'machakann/vim-swap'
"Plug 'powerman/AnsiEsc.vim'
Plug 'kana/vim-textobj-user' "something depends on this, but I don't remember what. either way, probably useful
"Plug 'kana/vim-textobj-entire'
Plug 'vim-scripts/camelcasemotion'
Plug 'michaeljsmith/vim-indent-object'
"Plug 'tpope/vim-speeddating'
"Plug 'tmhedberg/matchit' "Apparently ships with vim now?
"Plug 'kopischke/vim-fetch'
"Plug 'andrep/vimacs'
" }}}
" {{{ Features
"Plug 'theprimeagen/Vim-Be-Good', { 'do' : './install.sh' }
Plug 'dyng/ctrlsf.vim'
Plug 'simnalamburt/vim-mundo'
Plug 'kshenoy/vim-signature'
Plug 'godlygeek/tabular'
Plug 'majutsushi/tagbar'
let g:unicoder_cancel_normal = 1
Plug 'joom/latex-unicoder.vim'
Plug 'vim-scripts/loremipsum', { 'on' : 'Loremipsum' }
Plug 'junegunn/fzf'          , { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'metakirby5/codi.vim'   , { 'on' : 'Codi' }
Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
let g:ranger_replace_netrw = 1
    Plug 'rbgrouleff/bclose.vim' " for nvim
" }}}
" {{{ Completion, Snippets (broken?)
Plug 'Shougo/vimproc.vim'   , { 'do': 'make' }
Plug 'Shougo/echodoc.vim'
if has("python3")
  Plug 'Shougo/deoplete.nvim' , { 'do': ':UpdateRemotePlugins' }
  Plug 'SirVer/ultisnips'
endif
Plug 'honza/vim-snippets'
Plug 'ervandew/supertab'
" }}}
" {{{ Make
" somehow run in terminal?
Plug 'neomake/neomake'
" }}}
" {{{ Languages
"Plug 'autozimu/LanguageClient-neovim', { 'do': './install.sh', 'branch' : 'next'}
Plug 'rust-lang/rust.vim'  , { 'for' : 'rust' }
" {{{ Haskell
" TODO some sort of folding help
Plug 'dag/vim2hs' "Makes gf work on module names (broken?)
"Plug 'bitc/lushtags'       , { 'for' : 'haskell' }
"Plug 'eagletmt/ghcmod-vim' , { 'for' : 'haskell' }
" Plug 'bitc/vim-hdevtools' " Used with syntastic
"Plug 'parsonsmatt/intero-neovim'
" https://github.com/ucsd-progsys/liquid-types.vim
Plug 'ndmitchell/ghcid'   , { 'rtp': 'plugins/nvim', 'on' : 'Ghcid' }

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
" }}}
"Plug 'epdtry/neovim-coq'
"Plug 'idris-hackers/idris-vim'
Plug 'mattn/emmet-vim' | let g:user_zen_mode='a'
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
"Plug 'airblade/vim-gitgutter'
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
set hidden
set showcmd
set mouse=a
"set number
set numberwidth=2
set ruler
"so=7
set wildmenu wildmode=list:longest,full
set path+=**
set foldlevelstart=20

if &term=~'st-256colors' || &term=~'nvim'
    set termguicolors
endif
set bg=dark
let g:gruvbox_contrast_dark="hard"
colors gruvbox

function! MyOnBattery()
  if filereadable("/sys/class/power_supply/ADP0/online")
    return readfile('/sys/class/power_supply/ADP0/online') == ['0']
  endif
  return readfile('/sys/class/power_supply/AC/online') == ['0']
endfunction

" if MyOnBattery() | call neomake#configure#automake('w')
" else             | call neomake#configure#automake('nw', 700)
" endif

set list listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set fillchars+=vert:│
set dictionary+=/usr/share/dict/words
set smarttab expandtab shiftwidth=2 tabstop=2
set undofile undodir=~/.nvim/undo undolevels=10000 undoreload=100000
set ve=all

if getfperm(expand("%:p")) =~ "x" | set mp=./%
endif
" }}}
" {{{ Complete and snippets
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
inoremap <Nul> <c-j>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>

set omnifunc=syntaxcomplete#Complete
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

if has("python3")
  "let g:deoplete#enable_at_startup = 1
  "call deoplete#custom#set('ultisnips', 'matchers', ['matcher_fuzzy'])

  "let g:deoplete#sources = {}
  "let g:deoplete#sources._ = ['buffer', 'ultisnips']
  "let g:deoplete#sources.haskell = ['buffer', 'ultisnips', 'omni']
endif

let g:UltiSnipsExpandTrigger="<c-e>"
"inoremap <expr><c-e> pumvisible() ? "<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>" : "\<CR>"
" }}}
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
endif

" Tab navigation with Ctrl-Shift
if has("nvim")
  tnoremap <A-S-h> <C-\><C-n>:tabprev<CR>
  tnoremap <A-S-l> <C-\><C-n>:tabnext<CR>
  tnoremap <A-S-n> <C-\><C-n>:tabnew term://zsh<CR>
  tnoremap  
endif

nnoremap <A-S-h> :tabprev<CR>
nnoremap <A-S-l> :tabnext<CR>
nnoremap <A-S-n> :tabe<CR>
nnoremap <A-n>   :tabnew term://zsh<CR>

inoremap <A-S-h> :tabprev<CR>
inoremap <A-S-l> :tabnext<CR>
nnoremap <A-S-n> :tabe<CR>
inoremap <A-n> :tabnew term://zsh<CR>
" }}}

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

" {{{ Mappings
let mapleader = "\<Space>"

autocmd VimEnter SpeedDatingFormat! ^%v
autocmd VimEnter SpeedDatingFormat! %v
omap i, <Plug>(swap-textobject-i)
xmap i, <Plug>(swap-textobject-i)
omap a, <Plug>(swap-textobject-a)
xmap a, <Plug>(swap-textobject-a)
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
nmap [<Space> <Plug>unimpairedBlankUpk
nmap ]<Space> <Plug>unimpairedBlankDownj
nmap [, :tabp<CR>
nmap ]. :tabn<CR>
nnoremap  :CtrlSF
nmap <silent> <leader>u :MundoToggle<CR>

"clipboard
vnoremap <Leader>y "+y
vnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

nmap <leader>e :vsp ~/.config/nvim/init.vim<CR>
nmap <leader>f :Files<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>t :Tags<CR>
nmap <leader><leader>t :Windows<CR>

nnoremap <Leader><Leader>s :set spell<CR>
nnoremap <Leader>z 1z=

nnoremap <CR> :
nnoremap <Leader>w :w<CR>
nnoremap <Leader>v :w<CR>
nnoremap <Leader>r :w<CR>:make<CR>
imap <F9> <Esc>:w<CR>:make<CR>

nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
nnoremap <leader><cr> :noh<cr>
map Y y$
" Reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

call arpeggio#load()
Arpeggio imap jk <Esc>
Arpeggio map kl l
Arpeggio map hj h
Arpeggio map ui k
Arpeggio map nm j
Arpeggio map il gt
Arpeggio map hu gT
"Arpeggio map ag :Ag<CR>
Arpeggio imap ag <Esc>:Ag<CR>

inoremap <C-l> <Esc>:call unicoder#start(1)<CR>\
inore  :wqa
nore  :wqa

map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
" }}}

" Automatically source vimrc on save.
autocmd! bufwritepost $MYVIMRC source $MYVIMRC

" Graveyard
"Plug 'vimwiki/vimwiki'
"Plug 'FredKSchott/CoVim'
"Plug 'the-lambda-church/coquille' |  Plug 'def-lkb/vimbufsync'
"Plug 'jasonlong/lavalamp'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'vim-airline/vim-airline'
"  " set laststatus=2
"  let g:airline_powerline_fonts = 1
"  let g:airline#extensions#tabline#enabled = 1
"Plug 'vim-scripts/wombat256.vim'
"
"if filereadable("Session.vim")
"    source Session.vim
"endif
"autocmd WinEnter term://* startinsert
"Plug 'RelOps'

let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8 } }
let $FZF_DEFAULT_OPTS='--reverse'

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
