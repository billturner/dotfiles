# ENV
export PATH="$PATH:/usr/local/bin:/usr/local/share/npm/bin:$HOME/.rvm/bin:/usr/local/sbin"
export EDITOR=vim
export TERM="screen-256color"
export PHANTOMJS_BIN="~/src/qdw-web/node_modules/.bin/"

# general aliases
alias ls="ls -laGF"

# dev aliases
alias vim='nvim'

# git aliases
alias gs="git status"
alias gp="git pull"
alias gnew="git ls-files -o --exclude-standard | xargs git add"
alias gbr="git br"
alias gbrr="git br -a"
alias gl="git log --oneline --graph --decorate --all"

# tmux aliases
alias tnew="tmux new -s"
alias tko="tmux kill-session -t"
alias tat="tmux a -t"
alias tls="tmux ls"

# rails 3+ aliases
alias be="bundle exec"
alias beer="bundle exec rails"
alias brake="bundle exec rake"
alias brr="brake routes"
