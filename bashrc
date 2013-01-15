# ENV variables
# export GREP_OPTIONS="-i --exclude=\*.svn\*"
export GEM_HOME="/Library/Ruby/Gems/1.8"
export GEM_PATH="/System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/lib/ruby/gems/1.8"
export GEM_EDITOR="mate"
export PATH="/usr/local/bin:/Developer/usr/bin:/usr/local/pgsql/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
export LC_CTYPE=en_US.UTF-8
export EDITOR=mvim
export EVENT_NOKQUEUE=1
export HISTCONTROL=erasedups
export ARCHFLAGS='-arch x86_64'
export AUTOFEATURE=true
export RUBYOPT="rubygems"

shopt -s histappend

# db / service aliases
alias mystart="launchctl load -w ~/Library/LaunchAgents/com.mysql.mysqld.plist"
alias mystop="launchctl unload -w ~/Library/LaunchAgents/com.mysql.mysqld.plist"
# alias memstart="sudo launchctl load -w /Library/LaunchDaemons/com.danga.memcached.plist"
# alias memstop="sudo launchctl unload -w /Library/LaunchDaemons/com.danga.memcached.plist"
# alias mongostart="sudo launchctl load -w /Library/LaunchDaemons/org.mongodb.mongod.plist"
# alias mongostop="sudo launchctl unload -w /Library/LaunchDaemons/org.mongodb.mongod.plist"
alias poststart="launchctl load -w ~/Library/LaunchAgents/org.postgresql.postgres.plist"
alias poststop="launchctl unload -w ~/Library/LaunchAgents/org.postgresql.postgres.plist"

# helper aliases
alias ls="ls -laGF"
alias m="mvim"
alias mbash="vim -v ~/.bashrc"
alias rbash="source ~/.bashrc"
alias rmds="find . -name *.DS_Store -type f -exec rm {} \;"
alias vim="mvim -v"
alias wiki="cd ~/dev/wiki && gollum"

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
alias pryr="pry -r ./config/environment"
alias irp="pryr"

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

function gcof {
  git co feature/$1;
}

function gpu {
  if [ $(dirty_branch) -ne 0 ]
  then
    echo "Please commit changes first...";
    return 0;
  fi
  rebase_branch=${1:-develop};
  starting_branch=$(parse_git_branch);
  echo "Checking out $(rebase_branch) and updating...";
  # git co $rebase_branch;
  # git pull;
  # echo "Going back to $(starting_branch) branch and rebasing...";
  # git co $starting_branch;
  # git rebase $rebase_branch;
}

function dirty_branch {
  git_status="$(git status 2>/dev/null)"
  case "$git_status" in
    *deleted*)
      return 1;
      ;;
    *Untracked[[:space:]]files:*)
      return 1;
      ;;
    *modified:*)
      return 1;
      ;;
  esac
  return 0
}

function ruby_version {
    if [[ -f ~/.rvm/bin/rvm-prompt ]]; then
        local system=$(~/.rvm/bin/rvm-prompt s)
        local interp=$(~/.rvm/bin/rvm-prompt i)
        if [[ ! -n $system ]]; then
            # Don't show interpreter if it's just MRI
            case $interp in
                ruby) echo " [$(~/.rvm/bin/rvm-prompt v g)]" ;;
                *)    echo " [$(~/.rvm/bin/rvm-prompt i v g)]" ;;
            esac
        fi
    fi
}

function _rvm_ruby_version {
  local gemset=$(echo $GEM_HOME | awk -F'@' '{print $2}')
  [ "$gemset" != "" ] && gemset="@$gemset"
  local version=$(echo $MY_RUBY_HOME | awk -F'-' '{print $2}')
  [ "$version" == "1.8.7" ] && version=""
  local full="$version$gemset"
  [ "$full" != "" ] && echo " [$full]"
}

function parse_git_dirty {
  git diff --quiet || echo "*"
}

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}"$(parse_git_dirty))" 
}

# load ssh-agent
source ~/.ssh/agent.sh

# setup rvm
[[ -s "/Users/billturner/.rvm/scripts/rvm" ]] && source "/Users/billturner/.rvm/scripts/rvm"

# PS1='\u@\h:\w$(_rvm_ruby_version)$(parse_git_branch)$ '
source "$rvm_path/contrib/ps1_functions"
ps1_set


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
