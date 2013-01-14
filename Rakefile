#!/usr/bin/env ruby

require 'rake'
require 'fileutils'
require 'etc'

HOME_DIR = Etc.getpwuid.dir

namespace :setup do
  desc 'Determine if system is ready for all bundles'
  task :test do
    current_vim = `vim --version`
    current_vim_version = current_vim[/\b(\d+(\.\d+)?)\b/].to_f
    messages = ""
    messages << " * Vim version needs to be >= 7.3 (currently #{current_vim_version})\n" if current_vim_version < 7.3
    messages << " * Ruby support not built in (needed for Command-T plugin)\n" if current_vim[/-ruby/]
    messages << " * Python support not built in (needed for ConqueTerm plugin)\n" if current_vim[/-python\b/]
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
  # desc 'Cleanup and initial install'
  # task :initialize do
  #   puts 'Creating swp/ directory...'
  #   system %Q{ mkdir #{HOME_DIR}/.vim/swp }
  # end

  desc 'Install and update all submodules'
  task :submodules do
    puts "Initializing and updating plugins..."
    system %Q{ git submodule init }
    system %Q{ git submodule update }
  end

  # this assumes you at least have curl and a /usr/local/bin directory
  # desc 'Install the ack standalone if not already installed'
  # task :ack do
  #   if system %Q{ which ack }
  #     puts "Ack is installed"
  #   else
  #     puts "ack does not seem to be installed. Installing now."
  #     system %Q{ curl http://betterthangrep.com/ack-standalone > /tmp/ack } # to get around no sudo on 'curl'
  #     system %Q{ sudo mv /tmp/ack /usr/local/bin/ack }
  #     system %Q{ sudo chmod 0755 /usr/local/bin/ack }
  #   end
  # end

  # desc 'Install Exuberant-ctags for tags/taglist'
  # task :ctags do
  #   # determine platform
  #   if RUBY_PLATFORM[/darwin/]
  #     puts "Downloading, compiling, and installing exuberant ctags (uses 'sudo')"
  #     system %Q{ 
  #       mkdir tmp_install && curl -L http://downloads.sourceforge.net/project/ctags/ctags/5.8/ctags-5.8.tar.gz > tmp_install/ctags-5.8.tar.gz &&
  #       cd tmp_install && tar zxvf ctags-5.8.tar.gz && rm ctags-5.8.tar.gz &&
  #       cd ctags-5.8 && ./configure --prefix=/usr/local && make && sudo make install &&
  #       cd #{HOME_DIR}/.vim && rm -rf tmp_install
  #     }
  #   elsif RUBY_PLATFORM[/linux/] && `cat /etc/issue`[/Ubuntu|Debian/]
  #     puts "Installing exuberant-ctags package (uses 'sudo')"
  #     system %Q{ sudo apt-get install exuberant-ctags }
  #   else
  #     puts "Don't have an option to install. Sorry."
  #   end
  # end

  desc 'Add all symlinks'
  task :symlinks do
    %w{vimrc gvimrc ackrc ctags irbrc gemrc bash_profile bashrc vim}.each do |sym|
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
      system "ln -nfs #{HOME_DIR}/dotfiles/#{sym} #{HOME_DIR}/.#{sym}"
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
    bundle_dir = File.join(File.expand_path(File.dirname(__FILE__)), 'vim', 'bundle')
    puts "#{bundle_dir}"
    Dir.new(bundle_dir).entries.each do |bundle|
      unless ['.', '..'].include?(bundle)
        puts "Updating #{bundle}"
        system "cd #{File.join(bundle_dir, bundle)} && git pull"
      end
    end
  end
end

desc 'Perform all install tasks at once'
task :install => ['install:submodules', 'install:symlinks', 'clean:snipmate']

desc 'Do a full update - including building anything external'
task :update => ['update:submodules', 'clean:snipmate']
