#!/bin/sh

# AUTHOR:  Matt Simerson (matt@tnpi.net)
# DATE:    July, 2007
#
# INSTRUCTIONS
#   1. Install this script in your ~/.ssh directory as agent.sh
#
#      curl -o .ssh/agent.sh http://www.tnpi.net/computing/mac/agent.sh.txt
#      chmod 755 .ssh/agent.sh
#
#   2. Configure it to run when a new terminal window opens
#
#      echo 'source .ssh/agent.sh' >> ~/.bash_profile
#
#   3. Open new terminal/shell sessions
#
#   4. Enjoy
#
# GUI APPLICATIONS
#
#   If you use GUI apps that are ssh-agent aware, you will need to run the
#   script once and then log out and back in. Then your GUI apps will be
#   ssh-agent aware.

unset _sockfile

### begin SSH-AGENT SOCKET NOTES
#
# Setting _sockfile is an efficiency improvement. Rather than storing the 
# ssh socket file in /tmp/ssh-XXXXXXXXXX/agent.<ppid> and having to glob 
# to find it, we can store it in a fixed location so this script can find
# it more efficiently. Since all our shell/terminal windows will share the
# first ssh-agent process, there is no need for the random location.
#
# If you decide to alter the location, keep security in mind. You do not want
# others to have access to this socket. Your ~/.ssh directory is a great
# choice because its default permissions (600) are readable only by you.
#
# If you wish to keep the default /tmp behavior, simply comment out this
# setting
#
_sockfile="${HOME}/.ssh/agent.sock"
#
### end SSH-AGENT SOCKET NOTES

if [ "$0" != "-bash" ];
then
    echo '
OOPS! Did you mean to source this script? Try this:

    source ~/.ssh/agent.sh
'
fi


main()
{

# There are three states we check for
#
#   1. ssh-agent not running.
#
#      a) clean up any stale environment variables
#      b) launch the agent
#      c) configure our environment to use it
#      d) add our ssh keys
#
#   2. ssh-agent is running, but our environment variables are outdated.
#       (SSH_AGENT_PID is set but different than running ssh-agent pid)
#
#      a) clean up the stale environment variables
#      b) configure our environment to use the existing ssh-agent
#
#   3. ssh-agent is running
#
#      a) configure our environment to use the existing ssh-agent

    set_agent_pid

    if [ -z "${_agent_pid}" ];   # 1
    then
        cleanup_stale_agent
        start_ssh_agent
    elif [ ! -z $SSH_AGENT_PID ] && [ "${_agent_pid}" -ne $SSH_AGENT_PID ];
    then                         # 2
        cleanup_stale_agent
        discover_ssh_agent $_agent_pid
    else                         # 3
        discover_ssh_agent $_agent_pid
    fi
}

set_agent_pid()
{
    # this is expensive but reliable
    #echo "checking for ssh-agent process"
    _agent_pid=`ps auwxU $USER | grep ssh-agent | grep -v grep | awk '{ print $2 }' | tail -n1`
}

discover_ssh_agent()
{
    if [ -z "$1" ]; 
    then
        set_agent_pid
    fi

    if [ ! -z $_agent_pid ];
    then
        echo "ssh agent for $USER found at pid ${_agent_pid}."
        export SSH_AGENT_PID=${_agent_pid}

        # if _sockfile is not defined we must figure it out
        if [ -z "$_sockfile" ] || [ ! -e "$_sockfile" ];
        then
            _sock_pid=`echo "${_agent_pid} - 1" | bc`
            _sockfile=`/bin/ls /tmp/ssh-*/agent.${_sock_pid}`
        fi

        if [ ! -e "$_sockfile" ];
        then
            echo "ERROR: could not determine ssh-agent socket file for pid: $SSH_AGENT_PID"
        else
            export SSH_AUTH_SOCK=$_sockfile

            # make sure the Mac Login environment is configured
            if [ "`uname`" == "Darwin" ]; then
                setup_plist
            fi
        fi
    fi
}

start_ssh_agent() 
{
    if [ -z "$_sockfile" ];
    then
        echo "starting ssh-agent"
        ssh-agent > /dev/null
    else
        echo "starting ssh-agent -a $_sockfile"
        ssh-agent -a $_sockfile > /dev/null
    fi

    discover_ssh_agent

    if [ ! -z $SSH_AUTH_SOCK ];
    then
        # this will prompt the user to authenticate their ssh key(s)
        echo "adding ssh key(s) to agent"
        ssh-add
    fi
}

setup_plist() 
{
    _envdir="$HOME/.MacOSX"

    if [ ! -d $_envdir ]; then
        mkdir $_envdir
    fi

    _plist="$_envdir/environment.plist"

    if [ -e $_plist ];
    then
        if [ ! `grep "$_sockfile" $_plist` ]; then
            set +o noclobber
            write_plist    # check and update
        fi
    else
        write_plist        # create
    fi
}

write_plist()
{
    echo "updating Mac OS X environment"
    (
    cat <<EOXML
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN"
 "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
 <dict>
  <key>SSH_AUTH_SOCK</key>
  <string>${_sockfile}</string>
 </dict>
</plist>
EOXML
    ) > $_plist
}

cleanup_stale_agent() 
{
    echo "cleaning up stale ssh agent"
    # check the environment variable SSH_AGENT_PID as it could be set
    # despite the ssh-agent process being missing.
    if [ ! -z $SSH_AGENT_PID ];
    then
        #echo "cleaning up stale ssh agent pid: $SSH_AGENT_PID"
        unset SSH_AGENT_PID
    fi

    if [ ! -z $SSH_AUTH_SOCK ];
    then
        if [ ! -e $SSH_AUTH_SOCK ];
        then
            #echo "cleaning up stale agent socket file: $SSH_AUTH_SOCK"
            unset SSH_AUTH_SOCK
        fi
    fi
}

print_agent_info() {
    echo "pid :  $_agent_pid"
    echo "sock:  $_sockfile"
}


main
#print_agent_info