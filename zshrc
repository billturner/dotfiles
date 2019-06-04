export ZSH="/Users/bill.turner/.oh-my-zsh"
ZSH_THEME="pygmalion"

plugins=(
  git
)

# initialize OMZ
source $ZSH/oh-my-zsh.sh

# load general aliases, ENV settings, etc
# if [ -f ~/.env.sh ]; then source ~/.env.sh ; fi
source ~/.env.sh
