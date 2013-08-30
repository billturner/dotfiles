# chruby
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
RUBIES=(/opt/rubies/*)
chruby 2.0.0-p247

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi
