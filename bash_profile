# chruby
# if [ -f /usr/local/share/chruby/chruby.sh ]; then
#   source /usr/local/share/chruby/chruby.sh
#   RUBIES=(~/.rubies/*)
#   chruby 2.2.2
#   source /usr/local/share/chruby/auto.sh
# fi
export NVM_DIR="$HOME/.nvm"
if [ -f /usr/local/opt/nvm/nvm.sh ]; then
  source /usr/local/opt/nvm/nvm.sh
fi

if [ -f ~/.bashrc ]; then source ~/.bashrc ; fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# added by Anaconda3 5.2.0 installer
export PATH="/anaconda3/bin:$PATH"
