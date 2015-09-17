# chruby
if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  RUBIES=(~/.rubies/*)
  chruby 2.2.3
  source /usr/local/share/chruby/auto.sh
fi

if [ -f /usr/local/opt/nvm/nvm.sh ]; then
  source /usr/local/opt/nvm/nvm.sh
fi

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi
