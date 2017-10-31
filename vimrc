set nocompatible
filetype off

" vim-plug config
call plug#begin('~/.vim/plugged')

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" General coding
Plug 'scrooloose/syntastic'
Plug 'mtscout6/syntastic-local-eslint.vim'

" Javascript
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'isRuslan/vim-es6'
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

" Finish Vundle
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
  colorscheme base16-tomorrow
endif

" filetype/encoding
filetype plugin on
filetype indent on
set ff=unix
set encoding=utf-8
set fileencoding=utf-8
set termencoding=utf-8
scriptencoding utf-8

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
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.pdf,.DS_Store
set wildignore+=.hg,.svn,.git
set wildignore+=*.log

" navigation/editing helpers
imap <c-e> <c-o>$
imap <c-a> <c-o>^
nmap j gj
nmap k gk
nnoremap <Leader>ll :set list<CR>
map <Leader>q <c-w>q
cmap w!! w !sudo tee > /dev/null %  " write/save file with sudo
set pastetoggle=<F2>

" stop ex mode
:nnoremap Q <Nop>

set hidden
if exists("&colorcolumn")
  set colorcolumn=80,120
endif
set number
set relativenumber
" nnoremap <leader><leader> <C-^>
set backspace=indent,eol,start
set list
set listchars=tab:▸\ ,eol:¬

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
  " various ruby file types
  autocmd BufNewFile,BufRead {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby
  " javascript formatting
  autocmd BufNewFile,BufRead *.ts,*.json,*.es6,*.jsx set ft=javascript
  " markdown
  autocmd BufNewFile,BufRead *.markdown,*.mkd,*.md set ft=markdown
  autocmd FileType markdown setlocal wrap linebreak nolist
endif

"
" PLUGIN CONFIGS
"
" syntastic:
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_ruby_checkers = ['rubocop', 'mri']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_html_tidy_ignore_errors = ["is not recognized!", "discarding unexpected", " proprietary attribute ", "unescaped &"]
" let g:syntastic_eruby_ruby_quiet_messages = {'regex': 'possibly useless use of a variable in void context'}

" ctrlp:
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>

let g:ctrlp_custom_ignore = '_site\|dist\|bower_components\|build\|bundle\|tmp\|coverage\|vendor\|node_modules'
let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_depth=40
let g:ctrlp_working_path_mode='r'
let g:ctrlp_show_hidden=1

" Nerdtree
map <leader>n :NERDTreeToggle<CR>
map <leader>f :NERDTreeFind<CR>

let g:NERDTreeWinPos="left"
let g:NERDTreeShowHidden=1
let g:NERDTreeMinimalUI=1
let g:NERDTreeWinSize=40
au Filetype nerdtree setlocal nolist

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" vim-rails
let g:rails_statusline=0

" vim-rubytest
nmap <Leader>y <Plug>RubyTestRun
nmap <Leader>Y <Plug>RubyFileRun

let g:rubytest_in_quickfix = 0
let g:rubytest_cmd_test = "bundle exec spring testunit %p"
let g:rubytest_cmd_testcase = "bundle exec spring testunit %p -n '/%c/'"
let g:rubytest_cmd_spec = "bundle exec rspec '%p'"
let g:rubytest_cmd_example = "bundle exec rspec '%p' -l '%c'"

" Ack/Silver Searcher
if executable('ag')
  let g:ackprg = 'ag --nocolor --nogroup'
endif
nnoremap <leader>a :Ack<Space>
nnoremap <Leader>A :Ack "<C-r><C-w>"<CR>

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
