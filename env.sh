# ENV
export EDITOR=vim
export TERM="screen-256color"
export PHANTOMJS_BIN="~/src/qdw-web/node_modules/.bin/"
export PGDATA="/usr/local/var/postgres"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

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
