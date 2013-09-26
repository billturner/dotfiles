# chruby
source /usr/local/share/chruby/chruby.sh
RUBIES=(/opt/rubies/*)
chruby 2.0.0-p247
source /usr/local/share/chruby/auto.sh

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi
