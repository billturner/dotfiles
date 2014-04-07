# ENV variables
export GEM_EDITOR="vim"
export PATH="/usr/local/bin:/usr/local/heroku/bin:/usr/local/share/npm/bin:/usr/local/pgsql/bin:/usr/local/sbin:$PATH"
export LC_CTYPE=en_US.UTF-8
export EDITOR=vim
export EVENT_NOKQUEUE=1
export HISTCONTROL=erasedups
export ARCHFLAGS='-arch x86_64'
export AUTOFEATURE=true
export TERM="screen-256color"

shopt -s histappend

# db / service aliases
alias poststart="launchctl load -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist"
alias poststop="launchctl unload -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist"

# helper aliases
alias ls="ls -laGF"
alias mbash="vim ~/.bashrc"
alias rbash="source ~/.bashrc"
alias rmds="find . -name *.DS_Store -type f -exec rm {} \;"

# tmux aliases
alias tmux="tmux -2"
alias tmkill="tmux kill-session -t"
alias tmls="tmux ls"
alias tmnew="tmux new -s"
alias tmat="tmux attach-session -t"

# git aliases
alias gs="git status"
alias gnew="git ls-files -o --exclude-standard | xargs git add"

# git flow aliases
alias gffs="git flow feature start"
alias gfff="git flow feature finish"
alias gffr="git flow feature rebase"

# rails 3 aliases
alias be="bundle exec"
alias r="be rails"
alias rg="be rails g"
alias rc="be rails c"
alias rs="be rails s"
alias brake="bundle exec rake"
alias brr="brake routes"

# various aliases
alias pubkey="cat ~/.ssh/id_dsa.pub | pbcopy"
alias pyhttp="python -m SimpleHTTPServer"

# for fat fingers
alias bim="vim"

# functions

dontindex () {
  touch $1/.metadata_never_index;
  sudo mdutil -E $1;
}

function tmux_colors {
  for i in {0..255} ; do
    printf "\x1b[38;5;${i}mcolour${i}\n"
  done
}

# tmux setups
function tmuxvert {
  DIR="${1:?Directory not provided}"
  NAME="${2:-editing}"

  if [ ! -d "$DIR" ]; then
    echo "No such directory"
  else
    cd "$DIR"

    tmux has-session -t "$NAME" 2>/dev/null
    if [ $? != 0 ]; then
      tmux new-session -d -s $NAME -n "EDIT"
      tmux split-window -t $NAME:1.0 -h

      tmux send-keys -t $NAME:1.0 "vim" C-m
    fi
    tmux select-pane -t $NAME:1.0
    tmux attach-session -d -t $NAME
  fi
}

function tmuxsplit {
  DIR="${1:?Directory not provided}"
  NAME="${2:-editing}"

  if [ ! -d "$DIR" ]; then
    echo "No such directory"
  else
    cd "$DIR"

    tmux has-session -t "$NAME" 2>/dev/null
    if [ $? != 0 ]; then
      tmux new-session -d -s $NAME -n "EDIT"
      tmux new-window -t $NAME:2 -n "tests/prompt"
      tmux split-window -t $NAME:2.0 -v

      tmux send-keys -t $NAME:1 "vim" C-m
      # tmux send-keys -t $NAME:2.0 "top pane" C-m
      # tmux send-keys -t $NAME:2.1 "bottom pane" C-m
    fi
    tmux select-window -t $NAME:1
    tmux attach-session -d -t $NAME
  fi
}

# prompt stuff
# * color tips from: http://blog.sanctum.geek.nz/bash-prompts/
COLOR_RED='\[\e[0;2;31m\]'
COLOR_GREEN='\[\e[32m\]'
COLOR_BLUE='\[\e[34m\]'
COLOR_RESET='\[\e[0m\]'

# git branch
function _git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

function _ruby_version {
  if (which ruby | grep -q ruby); then
    ruby -v | cut -d" " -f2
  fi
}

export PS1="\u@\h:\w $COLOR_RED\$(_ruby_version)$COLOR_RESET$COLOR_BLUE\$(_git_branch)$COLOR_RESET$ "

# load ssh-agent
if [ -f ~/.ssh/agent.sh ]; then source ~/.ssh/agent.sh ; fi

# include a personal and work rc files if found
if [ -f ~/.personal.bashrc ]; then source ~/.personal.bashrc ; fi
if [ -f ~/.work.bashrc ]; then source ~/.work.bashrc ; fi
