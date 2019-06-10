export ZSH=~/.oh-my-zsh

ZSH_THEME="pygmalion-virtualenv"

plugins=(
  brew
  bundler
  colorize
  docker
  git
  jsontools
  tmux
  virtualenv
)

# initialize OMZ
source $ZSH/oh-my-zsh.sh

# load general aliases, ENV settings, etc
if [ -f ~/.env.sh ]; then source ~/.env.sh ; fi
