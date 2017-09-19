" vim: foldmethod=marker
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
" {{{ Make
" somehow run in terminal?
Plug 'neomake/neomake'
" Only on AC power
"autocmd! BufWritePost * Neomake
"Plug 'dojoteef/neomake-autolint'
" }}}
" {{{ Completion, Snippets, FZF
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'ervandew/supertab'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" }}}
" {{{ Tweaks
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
"Plug 'vim-indent-object'
Plug 'tmhedberg/matchit'
Plug 'vim-scripts/argtextobj.vim'
Plug 'kana/vim-niceblock'
Plug 'zirrostig/vim-schlepp'
Plug 'kopischke/vim-fetch'
Plug 'machakann/vim-swap'
"Plug 'RelOps'
" }}}
" {{{ Features
Plug 'dyng/ctrlsf.vim'
Plug 'vim-scripts/loremipsum', { 'on' : 'LoremIpsum' }
Plug 'simnalamburt/vim-mundo'
"Plug 'godlygeek/tabular'
Plug 'metakirby5/codi.vim'
Plug 'majutsushi/tagbar'
" }}}
" {{{ Languages
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/echodoc.vim'
" {{{ Haskell
Plug 'bitc/lushtags'
" https://github.com/ucsd-progsys/liquid-types.vim
Plug 'glittershark/vim-hare'
Plug 'eagletmt/ghcmod-vim'
"" Superceeded by language-client-server
"" Plug 'eagletmt/neco-ghc'
" Plug 'bitc/vim-hdevtools' " Used with syntastic
" Plug 'myfreeweb/intero.nvim'
" Plug 'mpickering/hlint-refactor-vim'
" }}}
Plug 'epdtry/neovim-coq'
Plug 'idris-hackers/idris-vim'
Plug 'mattn/emmet-vim' | let g:user_zen_mode='a'
" {{{ orgmode
Plug 'Detegr/vim-orgmode'
Plug 'dhruvasagar/vim-table-mode'
Plug 'vim-scripts/utl.vim'
Plug 'itchyny/calendar.vim' " mattn/calendar.vim?  Plug 'vim-scripts/calendar.vim'
Plug 'vim-scripts/SyntaxRange'
Plug 'tpope/vim-speeddating'
" }}}
" {{{ git
Plug 'jreybert/vimagit', {'on': 'Magit'}
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive' "for fzf
" Plug 'tweekmonster/gitbusy.vim'
" }}}
Plug 'vim-scripts/LaTeX-Suite-aka-Vim-LaTeX', { 'for': 'tex' }
Plug 'vim-pandoc/vim-pandoc'
"}}}
Plug 'morhetz/gruvbox'
call plug#end() | if exists("doinstall") | PlugInstall | endif
" }}}

" {{{ Options
"syntax enable
set hidden
set showcmd
set mouse=a
"set number
set numberwidth=2
set ruler so=7
set wildmenu wildmode=list:longest,full
set path+=**

if &term=~'st-256colors'
    set termguicolors!
endif
set bg=dark
colors gruvbox

set list listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set fillchars+=vert:│
set dictionary+=/usr/share/dict/words
set smarttab expandtab shiftwidth=2 tabstop=2
set undofile undodir=~/.nvim/undo undolevels=10000 undoreload=100000
" }}}
" {{{ Complete and snippets
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
inoremap <Nul> <c-j>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>

set omnifunc=syntaxcomplete#Complete
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

let g:deoplete#enable_at_startup = 1
call deoplete#custom#set('ultisnips', 'matchers', ['matcher_fuzzy'])

"let g:deoplete#sources = {}
"let g:deoplete#sources._ = ['buffer', 'ultisnips']
"let g:deoplete#sources.haskell = ['buffer', 'ultisnips', 'omni']

let g:UltiSnipsExpandTrigger="<c-e>"
"inoremap <expr><c-e> pumvisible() ? "<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>" : "\<CR>"
" }}}
" {{{ Windows and Tabs
" let terminal resize scale the internal windows
autocmd VimResized * :wincmd =

" Window Navigation with Alt

nnor <A-h> <C-w>h | nnor <A-j> <C-w>j | nnor <A-k> <C-w>k | nnor <A-l> <C-w>l
inor <A-h> <Esc><C-w>h | inor <A-j> <Esc><C-w>j | inor <A-k> <Esc><C-w>k | inor <A-l> <Esc><C-w>l
if has("nvim")
  tnoremap <A-h> <C-\><C-n><C-w>h | tnoremap <A-j> <C-\><C-n><C-w>j | tnoremap <A-k> <C-\><C-n><C-w>k | tnoremap <A-l> <C-\><C-n><C-w>l
endif

" Tab navigation with Ctrl-Shift
if has("nvim")
  tnoremap <A-S-k> <C-\><C-n>:tabprev<CR>
  tnoremap <A-S-j> <C-\><C-n>:tabnext<CR>
  tnoremap <A-S-n> <C-\><C-n>:tabnew term://zsh<CR>
  tnoremap  
endif

nnoremap <A-S-k> :tabprev<CR>
nnoremap <A-S-j> :tabnext<CR>
nnoremap <A-S-n> :tabnew term://zsh<CR>

inoremap <A-S-k> :tabprev<CR>
inoremap <A-S-j> :tabnext<CR>
inoremap <A-S-n> :tabnew term://zsh<CR>

autocmd WinEnter term://* startinsert
" }}}
"{{{ Language Client
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'haskell': ['hie', '--lsp'],
    \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1
"}}}
" {{{ Coq
" noremap <F5> :CoqLaunch<CR>
" au FileType coq call coquille#FNMapping()
" }}}}
" Haskell {{{

" map <silent> <leader>o :call hlintRefactorVim#ApplyOneSuggestion()<CR>
" map <silent> <leader>a :call hlintRefactorVim#ApplyAllSuggestions()<CR>
autocmd FileType haskell map <buffer><silent> <leader>g :GhcModSigCodegen<CR>
autocmd FileType haskell map <buffer><silent> <leader>s :GhcModSplitFunCase<CR>
autocmd FileType haskell map <buffer><silent> <leader>t :GhcModTypeInsert<CR>
"map <silent> <leader>r :Hrename
"         }}}
" orgmode {{{
nmap  <localleader>cc
let g:org_agenda_files = ['~/org/*.org']
"         }}}
" {{{ latex
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat = 'pdf'
" }}}

let mapleader = "\<Space>"

" Automatically source vimrc on save.
autocmd! bufwritepost $MYVIMRC source $MYVIMRC

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
"nmap <leader>gb :GitBusy

vnoremap <Leader>y "+y
vnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

nmap <leader>e :vsp ~/.config/nvim/init.vim<CR>

nnoremap <Leader><Leader>s :set spell<CR>
nnoremap <Leader>z 1z=

nnoremap <Leader>v :w<CR>
nnoremap <Leader>r :w<CR>:make<CR>
imap <F9> <Esc>:w<CR>:make<CR>

nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
nnoremap <leader><cr> :noh<cr>
map Y y$
" Reselect visual block after indent
vnoremap < <gv
vnoremap > >gv


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

