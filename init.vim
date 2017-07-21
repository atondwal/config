" vim: fdm=marker foldenable sw=4 ts=4 sts=4
" "zo" to open folds, "zc" to close, "zn" to disable.

" {{{ Plugins
" {{{ Plug setup
se nocompatible

let doinstall=0
if has("nvim")
    let path=$HOME."/.config/nvim/autoload/plug.vim"
else
    let path=$HOME."/.vim/autoload/plug.vim"
endif

if !filereadable(path)
    echo "Installing Plug..."
    echo ""
    silent exec "!curl -fLo ".path." --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    let doinstall=1
endif

call plug#begin('~/.config/nvim/autoload/plugged')
" }}}
" {{{ Make
Plug 'neomake/neomake' | Plug 'dojoteef/neomake-autolint'
"autocmd! BufWritePost * Neomake
" }}}
" {{{ Completion & Snippets
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
"Plug 'vimwiki/vimwiki'
Plug 'vim-scripts/loremipsum'
"Plug 'FredKSchott/CoVim'
Plug 'simnalamburt/vim-mundo'
"Plug 'godlygeek/tabular'
Plug 'metakirby5/codi.vim'
" }}}
" {{{ Languages
" {{{ Haskell
" https://github.com/ucsd-progsys/liquid-types.vim
" Plug 'glittershark/vim-hare'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
" Plug 'bitc/vim-hdevtools' " Used with syntastic
" Plug 'myfreeweb/intero.nvim'
" Plug 'mpickering/hlint-refactor-vim'
" }}}
" {{{ Coq
Plug 'epdtry/neovim-coq'
Plug 'idris-hackers/idris-vim'
" Plug 'the-lambda-church/coquille'
  " Plug 'def-lkb/vimbufsync'
" }}}
" {{{ HTML
Plug 'mattn/emmet-vim'
  let g:user_zen_mode='a'
" }}}
" {{{ English
"Plug 'LanguageTool'
" }}}
" {{{ orgmode
Plug 'Detegr/vim-orgmode'
let g:org_agenda_files = ['~/org/*.org']
Plug 'dhruvasagar/vim-table-mode'
Plug 'vim-scripts/utl.vim'
"Plug 'vim-scripts/calendar.vim'
Plug 'itchyny/calendar.vim' " mattn/calendar.vim?
Plug 'vim-scripts/SyntaxRange'
Plug 'tpope/vim-speeddating'
" }}}
" {{{ git
Plug 'jreybert/vimagit' | Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive' "for fzf
" Plug 'tweekmonster/gitbusy.vim'
" }}}
" {{{ latex
Plug 'vim-scripts/LaTeX-Suite-aka-Vim-LaTeX'
" }}}
"}}}
" {{{ Colors
"Plug 'jasonlong/lavalamp'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
  " set laststatus=2
  let g:airline_powerline_fonts = 1
  let g:airline#extensions#tabline#enabled = 1
Plug 'vim-scripts/wombat256.vim'
" }}}
" All of your Plugs must be added before the following line
call plug#end()            " required
if doinstall
   PlugInstall
endif
" }}}
" {{{ Options
syntax enable
set hidden
set showcmd
"set number
set numberwidth=2
set pastetoggle=<F7>
set so=7
set ruler
set wildmenu
set wildmode=list:longest,full
set path+=**

set list
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set fillchars+=vert:│

set dictionary+=/usr/share/dict/words

set smarttab
set expandtab
set shiftwidth=2
set tabstop=2
let mapleader = "\<Space>"

set undofile                " Save undo's after file closes
set undodir=~/.nvim/undo " where to save undo histories
set undolevels=10000         " How many undos
set undoreload=100000        " number of lines to save for undo
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
if has("nvim")
  tnoremap <A-h> <C-\><C-n><C-w>h
  tnoremap <A-j> <C-\><C-n><C-w>j
  tnoremap <A-k> <C-\><C-n><C-w>k
  tnoremap <A-l> <C-\><C-n><C-w>l
endif

nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

inoremap <A-h> <C-w>h
inoremap <A-j> <C-w>j
inoremap <A-k> <C-w>k
inoremap <A-l> <C-w>l

" Tab navigation with Ctrl-Shift
if has("nvim")
  tnoremap <A-S-h> <C-\><C-n>:tabprev<CR>
  tnoremap <A-S-l> <C-\><C-n>:tabnext<CR>
  tnoremap <A-S-n> <C-\><C-n>:tabnew term://zsh<CR>
  tnoremap  
endif
nnoremap <A-S-h> :tabprev<CR>
nnoremap <A-S-l> :tabnext<CR>
nnoremap <A-S-n> :tabnew term://zsh<CR>

inoremap <A-S-h> :tabprev<CR>
inoremap <A-S-l> :tabnext<CR>
inoremap <A-S-n> :tabnew term://zsh<CR>

" Automatically enter insert mode on terminals
" autocmd BufWinEnter,WinEnter term://* startinsert
autocmd WinEnter term://* startinsert
" }}}
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
"         }}}
" {{{ latex
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat = 'pdf'
" }}}

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
nnoremap  :CtrlSF
nmap <silent> <leader>u :MundoToggle<CR>
nmap <leader>gb :GitBusy

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

  colorscheme wombat256mod
  " Adjust signscolumn and syntastic to match wombat
  hi! link SignColumn LineNr
  hi! link SyntasticErrorSign ErrorMsg
  hi! link SyntasticWarningSign WarningMsg
" colors desert
hi Search cterm=NONE ctermfg=grey ctermbg=blue
hi Folded cterm=NONE ctermfg=3 ctermbg=234
hi VertSplit cterm=NONE
" au FileType org color desert
autocmd BufCreate term://* set readonly
let g:airline_theme='raven'
