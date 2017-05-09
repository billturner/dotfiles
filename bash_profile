# chruby
# if [ -f /usr/local/share/chruby/chruby.sh ]; then
#   source /usr/local/share/chruby/chruby.sh
#   RUBIES=(~/.rubies/*)
#   chruby 2.2.2
#   source /usr/local/share/chruby/auto.sh
# fi

if [ -f ~/.nvm/nvm.sh ]; then
  source ~/.nvm/nvm.sh
fi

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
