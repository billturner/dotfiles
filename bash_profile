# chruby
source /usr/local/share/chruby/chruby.sh
RUBIES=(~/.rubies/*)
chruby 2.0.0-p353
source /usr/local/share/chruby/auto.sh

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi
