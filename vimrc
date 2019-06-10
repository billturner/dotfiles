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

" autocomplete
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}

" Javascript
Plug 'isRuslan/vim-es6'
Plug 'elzr/vim-json'
Plug 'kchmck/vim-coffee-script'
Plug 'sheerun/vim-polyglot'

" Ruby/Rails
Plug 'tpope/vim-rails'
Plug 'janx/vim-rubytest'
Plug 'nelstrom/vim-textobj-rubyblock'

" Design/Navigation
Plug 'ctrlpvim/ctrlp.vim'
" Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar'
Plug 'mileszs/ack.vim'
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
inoremap jk <esc>

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

" coc.vim
let g:ruby_host_prog = '~/.rvm/rubies/ruby-2.4.2/bin/ruby'
let g:coc_node_path = "/usr/local/bin/node"
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" ale:
let g:ale_linters = {
\ 'javascript': ['eslint', 'prettier'],
\ 'python': ['pep8'],
\ 'ruby': ['rubocop', 'ruby']
\}
let g:ale_fixers = {
\ 'python': ['autopep8']
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

" Fzf
" nnoremap <Leader>t :Files<CR>
" nnoremap <Leader>b :Buffers<CR>
" command! -bang -nargs=* Rg
"   \ call fzf#vim#grep(
"   \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
"   \   <bang>0 ? fzf#vim#with_preview('up:60%')
"   \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"   \   <bang>0)

" ctrlp:
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>

let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_depth=100
let g:ctrlp_working_path_mode=''
let g:ctrlp_show_hidden=1
let g:ctrlp_max_files=0

" if ripgrep is installed, let's use it
" if executable('rg')
"   set grepprg=rg\ --color=never
"   let g:ackprg = 'rg --vimgrep'
"   let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
"   let g:ctrlp_use_caching = 0
" endif

" let us use ag again
" if executable('ag')
"   set grepprg=ag\ --nogroup\ --nocolor
"   let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
"   let g:ctrlp_use_caching = 0
" endif

" delimitMate
let g:delimitMate_expand_cr = 1

" netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 0
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_fastbrowse = 0
autocmd FileType netrw set nolist
autocmd FileType netrw setl bufhidden=wipe
map <leader>n :Explore<CR>

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
