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
2. Install homebrew packages: `brew install vim ctags the_silver_searcher chruby ruby-install`

Initial Ubuntu setup
--------------------

1. Install needed packages: `sudo apt-get install vim-nox ctags silversearcher-ag`

Final setup
-----------

**General**

To install the symbolic links, you will need some version of Ruby installed. To put the symbolic links in place:

    cd ~/.dotfiles
    rake install:symlinks

If you have a file (like a `.vimrc`) already in your root directory, it will be backed up by adding the extension `.old` to it before the new symbolic link is added.

**Vim**

Install the Vundle plugin:

    cd ~/.dotfiles
    mkdir vim/bundle
    git clone https://github.com/gmarik/Vundle.vim.git vim/bundle/Vundle.vim

Start up Vim, and install plugins via `:PluginInstall`
