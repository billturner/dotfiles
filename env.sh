# ENV
export EDITOR=vim
export TERM="screen-256color"
export PHANTOMJS_BIN="~/src/qdw-web/node_modules/.bin/"
export PGDATA="/usr/local/var/postgres"
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# general aliases
alias ls="ls -laGF"

# dev aliases
# alias vim='nvim'

# git aliases
alias gs="git status"
alias gp="git pull"
alias gnew="git ls-files -o --exclude-standard | xargs git add"
alias gbr="git br"
alias gbrr="git br -a"
alias gl="git log --oneline --graph --decorate --all"

export PATH="$PATH:~/.rbenv/shims:~/.rbenv/bin:/usr/local/opt/postgresql@10/bin:/usr/local/bin:/usr/local/share/npm/bin:/usr/local/sbin"

# rbenv
eval "$(rbenv init -)"
