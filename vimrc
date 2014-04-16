set nocompatible
filetype off

" set up pathogen
execute pathogen#infect()

filetype plugin indent on

" initial config
if has("syntax")
  syntax on
endif
let mapleader=","

" colorscheme
if &t_Co >= 256 || has("gui_running")
  set background=dark
  " solarized options
  " let g:solarized_visibility="normal"
  " let g:solarized_contrast="normal"
  " colorscheme solarized
  " colorscheme base16-default
  colorscheme lucius
  " colorscheme seoul256
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
nnoremap <leader><leader> <C-^>
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
  " json formatting
  autocmd BufNewFile,BufRead *.json set ft=javascript
  " markdown
  autocmd BufNewFile,BufRead *.markdown,*.mkd,*.md set ft=markdown
  autocmd FileType markdown setlocal wrap linebreak nolist
  " textile
  autocmd BufNewFile,BufRead *.textile set ft=textile
  autocmd FileType textile setlocal wrap linebreak nolist
endif

"
" PLUGIN CONFIGS
"
" ctrlp:
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>

let g:ctrlp_custom_ignore = '_site\|bundle\|tmp\|coverage'
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
let NERDTreeShowHidden=1
let NERDTreeMinimalUI=1
au Filetype nerdtree setlocal nolist

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" vim-rails
let g:rails_statusline=1

" vim-rubytest
nmap <Leader>y <Plug>RubyTestRun
nmap <Leader>Y <Plug>RubyFileRun

let g:rubytest_in_quickfix = 0
let g:rubytest_cmd_test = "ruby %p"
let g:rubytest_cmd_testcase = "ruby %p -n '/%c/'"
let g:rubytest_cmd_spec = "bundle exec rspec '%p'"
let g:rubytest_cmd_example = "bundle exec rspec '%p' -l '%c'"

" Silver Searcher
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
  let g:ctrlp_use_caching=0
  nnoremap <leader>a :Ag<Space>
  " bind K to grep word under cursor
  nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
else
  " ack.vim
  nnoremap <leader>a :Ack<Space>
endif

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
let g:airline_detect_whitespace = 0
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

" vim-mustache-handlebars
let g:mustache_abbreviations = 1

" use matchit
runtime macros/matchit.vim
