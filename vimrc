set nocompatible
filetype off

" vim-plug config
call plug#begin('~/.vim/plugged')

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'idanarye/vim-merginal'

" Syntax checking
Plug 'w0rp/ale'

" Javascript
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'isRuslan/vim-es6'
Plug 'elzr/vim-json'
Plug 'mxw/vim-jsx'

" Ruby/Rails
Plug 'tpope/vim-rails'
Plug 'janx/vim-rubytest'
Plug 'nelstrom/vim-textobj-rubyblock'

" Design/Navigation
Plug 'ctrlpvim/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'mileszs/ack.vim'
Plug 'scrooloose/nerdtree'
Plug 'easymotion/vim-easymotion'
Plug 'chriskempson/base16-vim'

" Snippets
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'

" General
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'kana/vim-textobj-user'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-surround'
Plug 'tommcdo/vim-lion'

" Finish vim-plug
call plug#end()

filetype plugin indent on

" initial config
if has("syntax")
  syntax on
endif
let mapleader=","

" colorscheme
if &t_Co >= 256 || has("gui_running")
  set background=dark
  colorscheme base16-oceanicnext
endif

" filetype/encoding
filetype plugin on
filetype indent on
set ff=unix
set encoding=utf-8
set fileencoding=utf-8
set termencoding=utf-8
scriptencoding utf-8

" code folding
set foldmethod=syntax
set foldlevel=99
nnoremap <Space> za
vnoremap <Space> za

" tabs
set autoindent
set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

" statusline
set laststatus=2

if has("statusline")
  " hi StatusLine ctermbg=145 ctermfg=Black
  set statusline=""
  set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline()\ :''}
  set statusline+=\ %f  " filename
  set statusline+=\ %m  " modified?
  set statusline+=\ %y  " filetype
  set statusline+=\ %r  " read-only?
  set statusline+=%= " right side of statusline starts
  set statusline+=\ col:%c,
  set statusline+=\ line\ %l\ of\ %L\ [%P]
  set statusline+=\ %#warningmsg#
  set statusline+=%{exists('g:loaded_syntastic_plugin')?SyntasticStatuslineFlag():''}
  set statusline+=%*
  set statusline+=\ 
endif

" searching/tab completion
nnoremap <CR> :noh<CR><CR>

set hlsearch
set incsearch
set ignorecase
set smartcase
set wildmenu
set wildmode=list:longest,list:full

" ignore image and doc files
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.pdf,*.doc,*.docx,*.xls,*.xlsx
" ignore font files
set wildignore+=*.ttf,*.otf,*.woff,*.woff2,*.eot
" ignore annoying mac files
set wildignore+=.DS_Store,.localized
" ignore repository configs
set wildignore+=.hg,.svn,.git
" ignore logfiles in general
set wildignore+=*.log
" ignore build directories
set wildignore+=*/build/**,*/dist/**,*/target/**
" ignore other things
set wildignore+=*/vendor/*,*/node_modules/**,*/bower_components/**,*/tmp/**,*/coverage/**

" navigation/editing helpers
imap <c-e> <c-o>$
imap <c-a> <c-o>^
nmap j gj
nmap k gk
map <Leader>q <c-w>q
cmap w!! w !sudo tee > /dev/null %  " write/save file with sudo
set pastetoggle=<F2>
nnoremap <Leader>ll :setlocal list! \| setlocal relativenumber! \| setlocal number!<CR>

" folding
" set foldmethod=indent
" set foldminlines=3
" set foldlevelstart=1

" stop ex mode
:nnoremap Q <Nop>

set hidden
" if exists("&colorcolumn")
"   set colorcolumn=80,120
" endif
set number
set relativenumber
" nnoremap <leader><leader> <C-^>
set backspace=indent,eol,start
set list
set listchars=tab:▸\ ,eol:¬
set clipboard=unnamed

" for fat fingers
cnoreabbrev W w
cnoreabbrev apl spl

" no swap/temp files
set noswapfile
set nobackup
set nowritebackup

" Helpful commands
command! Jsonf %!python -m json.tool

" Other filetypes
if has("autocmd")
  " ruby
  autocmd BufNewFile,BufRead {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} setlocal ft=ruby
  " javascript
  autocmd BufNewFile,BufRead *.ts,*.json,*.es6,*.jsx setlocal ft=javascript
  autocmd BufNewFile,BufRead *.vue setlocal filetype=vue.html.javascript.css
  " markdown
  autocmd BufNewFile,BufRead *.markdown,*.mkd,*.md setlocal ft=markdown
  autocmd FileType markdown setlocal wrap linebreak nonumber norelativenumber nolist colorcolumn=
  autocmd FileType python setlocal shiftwidth=4 tabstop=4
endif

"
" PLUGIN CONFIGS
"
" vim-prettier (defaults):
" let g:prettier#config#print_width = 100
" let g:prettier#config#bracket_spacing = 'false'
" let g:prettier#config#trailing_comma = 'none'

" ale:
let g:ale_linters = {
\ 'javascript': ['eslint', 'prettier'],
\ 'ruby': ['rubocop', 'ruby']
\}
let g:ale_set_highlights = 0
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 0
let g:ale_lint_on_text_changed = 'never'
let g:ale_ruby_rubocop_options = '--display-cop-names --rails'
let g:ale_javascript_eslint_use_global = 0
let g:ale_echo_msg_format = '[%linter%]: %s'
let g:ale_set_highlights = 0

" vim-javascript:
let g:javascript_plugin_flow = 1

" vim-jsx:
let g:jsx_ext_required = 0

" ctrlp:
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>

let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_depth=40
let g:ctrlp_working_path_mode='r'
let g:ctrlp_show_hidden=1

" if ripgrep is installed, let's use it
if executable('rg')
  set grepprg=rg\ --color=never
  let g:ackprg = 'rg --vimgrep'
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif

" delimitMate
let g:delimitMate_expand_cr = 1

" Nerdtree
map <leader>n :NERDTreeToggle<CR>
map <leader>f :NERDTreeFind<CR>

let g:NERDTreeWinPos="left"
let g:NERDTreeShowHidden=1
let g:NERDTreeMinimalUI=1
let g:NERDTreeWinSize=30
au Filetype nerdtree setlocal nolist

" Don't just leave Nerdtree as only window
" via: https://stackoverflow.com/a/4319165
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" vim-rails
let g:rails_statusline=0

" vim-rubytest
nmap <Leader>y <Plug>RubyTestRun
nmap <Leader>Y <Plug>RubyFileRun

let g:rubytest_in_quickfix = 0
let g:rubytest_cmd_test = "ruby -I test %p"
let g:rubytest_cmd_testcase = "ruby -I test %p -n '/%c/'"
let g:rubytest_cmd_spec = "SKIP_SIMPLECOV=true bundle exec rspec '%p' --format progress"
let g:rubytest_cmd_example = "SKIP_SIMPLECOV=true bundle exec rspec '%p':'%c' --format progress"

" Ack/Silver Searcher/ripgrep
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
elseif executable('ag')
  let g:ackprg = 'ag --nocolor --nogroup'
endif
nnoremap <leader>a :Ack!<Space>
nnoremap <Leader>A :Ack! "<C-r><C-w>"<CR>

" vim-gitgutter
let g:gitgutter_realtime = 0
let g:gitgutter_eager = 0
" fix vim-gitgutter column color
highlight clear SignColumn

" tagbar & tags related
nnoremap <leader>r :TagbarToggle<CR>
nmap <Leader>R :!ctags --exclude=tmp --exclude=tags --exclude=coverage --exclude=.git --exclude=log --extra=+f -R *<CR><CR>
nmap <C-\> :tnext<CR>

" use matchit
runtime macros/matchit.vim
