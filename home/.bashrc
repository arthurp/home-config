#!/bin/bash
# -*- sh -*-

if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi
if [ -f /etc/bash.bashrc ]; then
    source /etc/bash.bashrc
fi

# Exit if we are not running in an interactive shell
case $- in
    *i*) ;;
      *) return;;
esac


READLINK=readlink
if command -v greadlink > /dev/null; then
    READLINK=greadlink
fi

source "$HOME/.homesick/repos/homeshick/homeshick.sh"

HOME_CONSOLE_REPO_HOME=`$READLINK -f "$HOME/.bashrc"`
HOME_CONSOLE_REPO_HOME=`dirname "$HOME_CONSOLE_REPO_HOME"`
export HOME_CONSOLE_REPO=`dirname "$HOME_CONSOLE_REPO_HOME"`

shopt -s histappend cmdhist checkwinsize
HISTCONTROL=ignoredups
HISTSIZE=4096

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

source "$HOME_CONSOLE_REPO/lib/prompt.sh"

# COMPLETIONS

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

complete -F _known_hosts ssh-tmux

source ~/.homesick/repos/homeshick/completions/homeshick-completion.bash

source "$HOME_CONSOLE_REPO/lib/commands.sh"

export EDITOR=nano
export SUDO_ASKPASS=ssh-askpass

if command -v java > /dev/null; then
    export JAVA_HOME="$(dirname $(dirname $($READLINK -f $(which java))) | sed 's@/jre$@@')"
fi

export ELECTRON_TRASH="trash-cli"
# export GTK_OVERLAY_SCROLLING=0

source "$HOME_CONSOLE_REPO/pathrc"

if [ -f $HOME/.bashrc.local ]; then
    source $HOME/.bashrc.local
fi

