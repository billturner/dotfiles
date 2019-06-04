#!/usr/bin/env ruby

require 'rake'
require 'fileutils'
require 'etc'

HOME_DIR = Etc.getpwuid.dir
DOTFILES_DIR = File.expand_path(File.dirname(__FILE__))

namespace :install do
  desc 'Add all symlinks'
  task :symlinks do
    %w{gitignore gitconfig vimrc gvimrc agignore ackrc ctags irbrc gemrc bash_profile bashrc tmux.conf railsrc psqlrc vim zshrc env.sh}.each do |sym|
      # first, move existing files and directories to NAME.old
      if File.symlink?("#{HOME_DIR}/.#{sym}")
        puts "Removing old symlink for .#{sym}"
        system "rm #{HOME_DIR}/.#{sym}"
      elsif File.exist?("#{HOME_DIR}/.#{sym}") || File.directory?("#{HOME_DIR}/.#{sym}")
        puts "Backing up existing .#{sym} to .#{sym}.old"
        system "mv #{HOME_DIR}/.#{sym} #{HOME_DIR}/.#{sym}.old"
      end
      # now, add the symlinks
      puts "Adding a symlink for #{sym}"
      system "ln -nfs #{DOTFILES_DIR}/#{sym} #{HOME_DIR}/.#{sym}"
    end

    # link personal rc file if found
    if File.exist?("#{DOTFILES_DIR}/personalrc")
      if File.symlink?("#{HOME_DIR}/.personalrc")
        puts "Removing old symlink for .personalrc"
        system "rm #{HOME_DIR}/.personalrc"
      end
      puts "Adding a symlink for .personalrc"
      system "ln -nfs #{DOTFILES_DIR}/personalrc #{HOME_DIR}/.personalrc"
    end
  end
end

desc 'Perform all install tasks at once'
task :install => ['install:symlinks']
