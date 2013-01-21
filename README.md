dotfiles
========

My collection of Bash, Vim, and other related configuration files. There is also a Rakefile for some common tasks.

A few notes on some of the Vim plugins used:

**snipmate.vim:** I remove the bundled snippets and have been rewriting just the ones I need in the `snippets/` directory.

The included rake tasks, with a brief explanation:

    ~$ rake -T
    rake clean:snipmate         # Clean out default snipmate snippets
    rake install                # Perform all install tasks at once
    rake install:helpers:mac    # Install ctags and ack on the Mac (using Homebrew)
    rake install:helpers:ubuntu # Install ctags and ack on the Mac (using Homebrew)
    rake install:submodules     # Install and update all submodules
    rake install:symlinks       # Add symlinks to .vimrc & .gvimrc
    rake setup:test             # Determine if system is ready for all bundles
    rake setup:ubuntu           # Install necessary Vim package on ubuntu
    rake update                 # Do a full update
    rake update:submodules      # Update git submodules
