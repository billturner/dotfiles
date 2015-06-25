dotfiles
========

My collection of Bash, Vim, and other related configuration files. There is also a Rakefile for some common tasks.

Cloning project
---------------

    cd ~
    git clone https://github.com/billturner/dotfiles.git .dotfiles

Initial Mac setup
-----------------

1. Install homebrew, via [their instructions](http://brew.sh/).
2. Install homebrew packages: `brew install vim ctags the_silver_searcher`

Initial Ubuntu setup
--------------------

1. Install needed packages: `sudo apt-get install ctags silversearcher-ag`

Final setup
-----------

**Vim**

Install the Vundle plugin:

    cd ~/.dotfiles
    mkdir vim/bundle
    git clone https://github.com/gmarik/Vundle.vim.git vim/bundle/Vundle.vim

Start up Vim, and install plugins in Vim via `:PluginInstall`
