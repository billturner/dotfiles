set nocompatible
filetype off

" Vundle config
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Vundle itself
Plugin 'gmarik/Vundle.vim'

" Git
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

" General coding
Plugin 'scrooloose/syntastic'

" Javascript
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'burnettk/vim-angular'
Plugin 'kchmck/vim-coffee-script'

" Ruby/Rails
Plugin 'tpope/vim-rails'
Plugin 'janx/vim-rubytest'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'tpope/vim-haml'

" Design/Navigation
Plugin 'bling/vim-airline'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vivkin/flatland.vim'
Plugin 'majutsushi/tagbar'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'easymotion/vim-easymotion'

" Snippets
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'

" General
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-endwise'
Plugin 'kana/vim-textobj-user'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-surround'

" Finish Vundle
call vundle#end()

filetype plugin indent on

" initial config
if has("syntax")
  syntax on
endif
let mapleader=","

" colorscheme
if &t_Co >= 256 || has("gui_running")
  set background=dark
  " iTerm2 theme base16-tomorrow.dark (16)
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
  set statusline=""
  set statusline+=%n:%f
  set statusline+=\ %m
  set statusline+=\ %r
  set statusline+=\ %y
  set statusline+=%= " right side of statusline starts
  if exists("g:loaded_fugitive")
    set statusline+=%{fugitive#statusline()}
  endif
  set statusline+=\ col:%c,
  set statusline+=\ line\ %l\ of\ %L\ [%P]
  set statusline+=\ %#warningmsg#
  set statusline+=%{SyntasticStatuslineFlag()}
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

" stop ex mode
:nnoremap Q <Nop>

set hidden
if exists("&colorcolumn")
  set colorcolumn=80,120
endif
set number
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

" Other filetypes
if has("autocmd")
  " various ruby file types
  autocmd BufNewFile,BufRead {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby
  " javascript formatting
  autocmd BufNewFile,BufRead *.json,*.es6 set ft=javascript
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
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_html_tidy_ignore_errors = ["is not recognized!", "discarding unexpected", " proprietary attribute ", "unescaped &"]
" let g:syntastic_eruby_ruby_quiet_messages = {'regex': 'possibly useless use of a variable in void context'}

" ctrlp:
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>

let g:ctrlp_custom_ignore = '_site\|bundle\|tmp\|coverage\|vendor\|node_modules'
let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_depth=40
let g:ctrlp_working_path_mode='r'
let g:ctrlp_show_hidden=1

" Nerdcommenter
let g:NERDSpaceDelims=1

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
let g:rails_statusline=1

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

" vim-airline
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ""
let g:airline_right_sep = ""
let g:airline_detect_modified = 0
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#hunks#enabled = 0
let g:airline_symbols = {}
let g:airline_symbols.branch = "⎇"
let g:airline_symbols.paste = "ρ"
let g:airline_section_c = "%m %f"
let g:airline_theme = "base16"

" tagbar & tags related
nnoremap <leader>r :TagbarToggle<CR>
nmap <Leader>R :!ctags --exclude=tmp --exclude=tags --exclude=coverage --exclude=.git --exclude=log --extra=+f -R *<CR><CR>
nmap <C-\> :tnext<CR>

" use matchit
runtime macros/matchit.vim
