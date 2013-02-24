#!/usr/bin/env ruby

require 'rake'
require 'fileutils'
require 'etc'

HOME_DIR = Etc.getpwuid.dir
DOTFILES_DIR = File.expand_path(File.dirname(__FILE__))

namespace :setup do
  desc 'Determine if system is ready for all bundles'
  task :test do
    current_vim = `vim --version`
    current_vim_version = current_vim[/\b(\d+(\.\d+)?)\b/].to_f
    messages = ""
    messages << " * Ruby support not built in (may be needed for some plugins)\n" if current_vim[/-ruby/]
    if messages.empty?
      puts "The current system Vim seems like it should support this configuration and all bundled plugins!\n"
    else
      puts "The current system Vim does not seem to be ready:\n#{messages}"
    end
  end

  desc 'Install necessary Vim package on ubuntu'
  task :ubuntu do
    puts "Installing vim-nox package"
    system "sudo apt-get install vim-nox"
  end
end

namespace :install do
  namespace :helpers do
    desc "Install the helper applications (via Homebrew)"
    task :mac do
      if system %Q{ which brew }
        puts "Installing ack and ctags"
        system %Q{ brew install ack ctags }
      else
        puts "Install homebrew first"
      end
    end
    desc "Install the helper applications (via apt-get)"
    task :ubuntu do
      puts "Installing ctags"
      system %Q{ sudo apt-get install ctags }
      puts "Installing ack-grep (and adding symlink to ack)"
      system %Q{ sudo apt-get install ack-grep }
      system %Q{ sudo dpkg-divert --local --divert /usr/bin/ack --rename --add /usr/bin/ack-grep }
    end
  end

  desc 'Install and update all submodules'
  task :submodules do
    puts "Initializing and updating plugins..."
    system %Q{ git submodule init }
    system %Q{ git submodule update }
  end

  desc 'Add all symlinks'
  task :symlinks do
    %w{gitignore gitconfig vimrc gvimrc ackrc ctags irbrc gemrc bash_profile bashrc vim}.each do |sym|
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

namespace :clean do
  desc 'Clean out default snipMate snippets'
  task :snipmate do
    snippets_dir = File.join(File.expand_path(File.dirname(__FILE__)), 'bundle', 'vim-snipmate', 'snippets')
    puts "Removing default snipMate snippets"
    FileUtils.rm Dir.glob(snippets_dir + "/*.snippets")
  end
end

namespace :update do
  desc 'Update git submodules'
  task :submodules do
    puts "Updating submodules"
    system "git submodule foreach git pull origin master"
  end
end

desc 'Perform all install tasks at once'
task :install => ['install:submodules', 'install:symlinks', 'clean:snipmate']

desc 'Do a full update - including building anything external'
task :update => ['update:submodules', 'clean:snipmate']
