# ENV variables
export GEM_EDITOR="vim"
export PATH="/Applications/VirtualBox.app/Contents/MacOS:/usr/local/bin:/usr/local/share/npm/bin:/usr/local/pgsql/bin:/usr/local/sbin:$PATH"
export LC_CTYPE=en_US.UTF-8
export EDITOR=vim
export EVENT_NOKQUEUE=1
export HISTCONTROL=erasedups
export ARCHFLAGS='-arch x86_64'
export AUTOFEATURE=true
export TERM="screen-256color"
export NVM_DIR=~/.nvm
export ANDROID_HOME=/usr/local/opt/android-sdk

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
alias mbash="vim ~/.bashrc"
alias rbash="source ~/.bashrc"
alias rmds="find . -name *.DS_Store -type f -exec rm {} \;"
alias npm-exec='PATH=$(npm bin):$PATH'

# tmux aliases
alias tmk="tmux kill-session -t"
alias tml="tmux ls"
alias tma="tmux attach -t"

# git aliases
alias gs="git status"
alias gnew="git ls-files -o --exclude-standard | xargs git add"

# git flow aliases
alias gffs="git flow feature start"
alias gfff="git flow feature finish"
alias gffr="git flow feature rebase"

# rails 3/4 aliases
alias be="bundle exec"
alias r="be rails"
alias rg="be rails g"
alias rc="be rails c"
alias rs="be rails s"
alias brake="bundle exec rake"
alias brr="brake routes"

# docker aliases
alias dm="docker-machine"
alias dms="docker-machine start"
alias dmst="docker-machine status"
alias dk="docker"

# tmux aliases
alias tnew="tmux new -s"
alias tkill="tmux kill-session -t"
alias tat="tmux a -t"
alias tls="tmux ls"

# various aliases
alias pubkey="cat ~/.ssh/id_dsa.pub | pbcopy"
alias pyhttp="python -m SimpleHTTPServer"

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
function _git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

function _ruby_version {
  if (which ruby | grep -q ruby); then
    ruby -v | cut -d" " -f2 | sed -e 's/p[0-9]*//g'
  fi
}

function _node_version {
  if (nvm | grep -q nvm); then
    nvm current | sed -e 's/^v//' -e 's/none/-/'
  fi
}

export PS1="\u@\h:\w $COLOR_YELLOW"[N@"\$(_node_version)"]"$COLOR_RESET$COLOR_RED"[R@"\$(_ruby_version)"]"$COLOR_RESET$COLOR_BLUE"[G@"\$(_git_branch)"]"$COLOR_RESET$ "

# include a personal and work rc files if found
if [ -f ~/.personal.bashrc ]; then source ~/.personal.bashrc ; fi
if [ -f ~/.work.bashrc ]; then source ~/.work.bashrc ; fi

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
