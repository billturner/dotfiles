# chruby
if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  RUBIES=(~/.rubies/*)
  chruby 2.1.2
  source /usr/local/share/chruby/auto.sh
fi

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi
