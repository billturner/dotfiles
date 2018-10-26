# ENV variables
export GEM_EDITOR="vim"
export PATH="/usr/local/bin:/usr/local/share/npm/bin:$HOME/.rvm/bin:/usr/local/sbin:$PATH"
export LC_CTYPE=en_US.UTF-8
export EDITOR=vim
export EVENT_NOKQUEUE=1
export HISTCONTROL=erasedups
export ARCHFLAGS='-arch x86_64'
export AUTOFEATURE=true
export TERM="screen-256color"
# export RUBYOPT="-W0"

shopt -s histappend

# db / service aliases
alias poststart="launchctl load -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist"
alias poststop="launchctl unload -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist"
alias mystart="mysql.server start"
alias mystop="mysql.server stop"
alias nginx.start='sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.nginx.plist'
alias nginx.stop='sudo launchctl unload /Library/LaunchDaemons/homebrew.mxcl.nginx.plist'
alias nginx.restart='nginx.stop && nginx.start'

# helper aliases
alias ls="ls -laGF"
alias ebash="vim ~/.bashrc"
alias sbash="source ~/.bashrc"
alias rmds="find . -name *.DS_Store -type f -exec rm {} \;"
alias npm-exec='PATH=$(npm bin):$PATH'

# tmux aliases
alias tnew="tmux new -s"
alias tko="tmux kill-session -t"
alias tat="tmux a -t"
alias tls="tmux ls"

# git aliases
alias gs="git status"
alias gp="git pull"
alias gnew="git ls-files -o --exclude-standard | xargs git add"
alias gbr="git br"
alias gbrr="git br -a"
alias gl="git log --oneline --graph --decorate --all"

# nvm aliases
alias nu="nvm use"
alias ni="nvm install"
alias nls="nvm ls"
alias nlsr="nvm ls-remote"

# rails 3/4 aliases
alias be="bundle exec"
alias beer="bundle exec rails"
alias brake="bundle exec rake"
alias brr="brake routes"

# docker aliases
alias dk="docker"

# various aliases
alias pubkey="cat ~/.ssh/id_rsa.pub | pbcopy"
alias pyhttp="python -m SimpleHTTPServer"
alias em="/usr/local/Cellar/emacs/25.2/Emacs.app/Contents/MacOS/Emacs -nw"

# for reassignment
# alias vim="nvim"

# for fat fingers
alias bim="vim"

# functions
function tmux_colors {
  for i in {0..255} ; do
    printf "\x1b[38;5;${i}mcolour${i}\n"
  done
}

# prompt stuff
# * color tips from: http://blog.sanctum.geek.nz/bash-prompts/
COLOR_RED='\[\e[0;2;31m\]'
COLOR_GREEN='\[\e[32m\]'
COLOR_YELLOW='\[\e[33m\]'
COLOR_BLUE='\[\e[34m\]'
COLOR_RESET='\[\e[0m\]'

# git branch
function _current_git_branch {
  local branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null);
  if [[ -n $branch ]];then
    echo " ($branch)"
  fi
}
export PS1="\u@\h:\w$COLOR_BLUE""\$(_current_git_branch)""$COLOR_RESET$ "

# include a personal and work rc files if found
if [ -f ~/.personal.bashrc ]; then source ~/.personal.bashrc ; fi
if [ -f ~/.work.bashrc ]; then source ~/.work.bashrc ; fi

# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
