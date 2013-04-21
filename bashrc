# ENV variables
# export GEM_HOME="/Library/Ruby/Gems/1.8"
# export GEM_PATH="/System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/lib/ruby/gems/1.8"
export GEM_EDITOR="vim"
export PATH="/usr/local/bin:/usr/local/share/npm/bin:/Developer/usr/bin:/usr/local/pgsql/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
export LC_CTYPE=en_US.UTF-8
export EDITOR=mvim
export EVENT_NOKQUEUE=1
export HISTCONTROL=erasedups
export ARCHFLAGS='-arch x86_64'
export AUTOFEATURE=true
# export RUBYOPT="rubygems"
export TERM="screen-256color"

shopt -s histappend

# db / service aliases
alias mystart="launchctl load -w ~/Library/LaunchAgents/com.mysql.mysqld.plist"
alias mystop="launchctl unload -w ~/Library/LaunchAgents/com.mysql.mysqld.plist"
alias poststart="launchctl load -w ~/Library/LaunchAgents/org.postgresql.postgres.plist"
alias poststop="launchctl unload -w ~/Library/LaunchAgents/org.postgresql.postgres.plist"

# helper aliases
alias ls="ls -laGF"
alias m="mvim"
alias mbash="vim ~/.bashrc"
alias rbash="source ~/.bashrc"
alias rmds="find . -name *.DS_Store -type f -exec rm {} \;"
alias vim="mvim -v"

# tmux aliases
alias tmux="tmux -2"
alias tmkill="tmux kill-session -t"
alias tmls="tmux ls"
alias tmnew="tmux new -s"
alias tmat="tmux attach-session -t"

# git aliases
alias gs="git status"
alias gdiff="git diff | mate -"
alias gnew="git ls-files -o --exclude-standard | xargs git add"
alias gitrb="git rebase --continue"

# git flow aliases
alias gffs="git flow feature start"
alias gfff="git flow feature finish"
alias gffr="git flow feature rebase"

# rails 3 aliases
alias r="rails"
alias rg="rails generate"
alias rc="rails console"
alias rs="rails server"
alias be="bundle exec"
alias brake="bundle exec rake"
alias brr="brake routes"
alias ss="script/server"
alias sc="script/console"
alias sg="script/generate"
alias rr="rake routes"

# testing aliases
alias rrtest="ruby -Itest"
alias rrspec="ruby -Ispec"

# rvm
alias rsys="rvm use system"
alias wr="which ruby"
alias rvminit="gem install bundler"

# various aliases
alias pubkey="cat ~/.ssh/id_dsa.pub | pbcopy"
alias vim="/Applications/MacVim.app/Contents/MacOS/vim"

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
function tmuxwork {
  cd ~/dev/work

  SIZE=${1:-normal}

  tmux has-session -t work 2>/dev/null
  if [ $? != 0 ]; then
    tmux new-session -d -s work -n "edit"
    if [ "$SIZE" == "wide" ]; then
      tmux split-window -t work:1.0 -h
      tmux send-keys -t work:1.0 "vim" C-m
      tmux new-window -t work:2 -n "server/test log"
      tmux split-window -t work:2.0 -h
      tmux send-keys -t work:2.0 "rs" C-m
      tmux send-keys -t work:2.1 "tail -f log/test.log" C-m
      tmux select-pane -t work:1.0
    else
      tmux new-window -t work:2 -n "server"
      tmux new-window -t work:3 -n "test log"
      tmux new-window -t work:4 -n "shell"

      tmux send-keys -t work:1 "vim" C-m
      tmux send-keys -t work:2 "rails server" C-m
      tmux send-keys -t work:3 "tail -f log/test.log" C-m
      tmux select-window -t work:1
    fi
  fi
  tmux attach-session -d -t work
}

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

# load ssh-agent
source ~/.ssh/agent.sh

# setup rvm
# [[ -s "/Users/billturner/.rvm/scripts/rvm" ]] && source "/Users/billturner/.rvm/scripts/rvm"

# PS1='\u@\h:\w$(_rvm_ruby_version)$(parse_git_branch)$ '
# source "$rvm_path/contrib/ps1_functions"
# ps1_set


# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# git branch
function _git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

function _ruby_version {
  if (which ruby | grep -q ruby); then
    ruby -v | cut -d ' ' -f2
  fi
}

# prompt
PS1='\u@\h:\w $(_ruby_version)$(_git_branch)$ '

# include a personal rc file if found
if [ -f ~/.personalrc ]; then source ~/.personalrc ; fi
