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

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME_CONSOLE_REPO/lib.sh"

shopt -s histappend cmdhist checkwinsize
HISTCONTROL=ignoredups
HISTSIZE=4096

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# PROMPT

if [[ -n "$TERM" && "$TERM" != "dumb" ]]; then
    function ee() {
        [[ -n "$PS_MODE" ]] && echo -n "\["
    }
    function de() {
        [[ -n "$PS_MODE" ]] && echo -n "\]"
    }

    function tp() {
        ee
        tput "$@"
        de
    }
    function setfg() {
        ee
        printf '\e[38;5;%dm' "$1"
        de
    }
    function setbg() {
        ee
        printf '\e[48;5;%dm' "$1"
        de
    }
    function ps_unicode() {
        ee
        tput sc
        de
        echo -n "@"
        ee
        tput rc
        printf "\u$1"
        de
    }
else
    function tp() {
        true
    }
    function setfg() {
        true
    }
    function setbg() {
        true
    }
fi

function git_ps1_info() {
    if git rev-parse --show-toplevel 2> /dev/null > /dev/null; then
        local GP="$(git rev-parse --show-toplevel)"
        local X="$(basename "$GP"):"
        echo -n " [$X$(git rev-parse --abbrev-ref HEAD 2> /dev/null)]"
    fi
}
function env_ps1_info() {
    if [ "$CONDA_PREFIX" ]; then
        echo -n " (conda $(basename "$CONDA_PREFIX"))"
    fi
    if [ "$VIRTUAL_ENV" ]; then
        echo -n " (venv $(basename "$VIRTUAL_ENV"))"
    fi
}
function ps1_user_color() {
    if [[ $UID -lt 1000 ]]; then
        setfg 9
    fi
}
function ps1_user() {
    local HOSTNAME_PS USER_PS EXT_PS
    if [[ "$SSH_CLIENT" ]]; then
        HOSTNAME_PS="@$(hostname)"
    fi
    if [[ "$(basename "$HOME")" != $(whoami) ]]; then
        USER_PS="$(whoami)"
    fi
    if [[ "$USER_PS$HOSTNAME_PS" ]]; then
        echo -n "$USER_PS$HOSTNAME_PS:"
    fi  
}

export PROMPT_DIRTRIM=3

PS_MODE=T

if [[ "$TERM" == xterm-* || "$TERM" == screen || "$TERM" == tmux ]] && [[ "$(uname)" != "Darwin" ]]; then
    SOLID_LEFT_WEDGE="$(ps_unicode E0B0)"
    EMPTY_LEFT_WEDGE="$(ps_unicode E0B1)"
else
    SOLID_LEFT_WEDGE=" "
    EMPTY_LEFT_WEDGE=">"
fi

export PS1="$(setbg 237)\$(ps1_user)$(setfg 10)\w$(setfg 7)\$(git_ps1_info)\$(env_ps1_info)$(tp sgr0)\n$(setbg 237)$(setfg 7)"'\$'"$(setfg 237; setbg 0)$SOLID_LEFT_WEDGE$(tp sgr0)"
export PS2=" $(setfg 251)$EMPTY_LEFT_WEDGE$(tp sgr0)"
export PS4=" $EMPTY_LEFT_WEDGE"

PS_MODE=""

# COMPLETIONS

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

complete -F _known_hosts ssh-install-homeshick ssh-tmux

source ~/.homesick/repos/homeshick/completions/homeshick-completion.bash

# COMMANDS

function tmux_attach_or_new() {
    tmux -2 new-session -A -s 0
}

alias t="tmux_attach_or_new"

TMUX_SCROLLBACK_BEGIN=-32768
if version_at_least "3.0" "$TMUX_VERSION"; then
    TMUX_SCROLLBACK_BEGIN=-
fi

function tmux_capture() {
    local FILE="$1"
    local used_tmp=false
    if [[ -z "$FILE" ]]; then 
        used_tmp=true
        FILE="$(mktemp -p /tmp "scrollback-XXXX")"
    fi
    tmux capture-pane -S $TMUX_SCROLLBACK_BEGIN -p > "$FILE"
    if $used_tmp; then
        mv --backup=numbered "$FILE" "$HOME/tmux_capture"
    fi
}

# ALIASES

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
fi

alias ll='ls -lh'
alias lla='ls -alh'
alias la='ls -AC'
alias l='ls -C'
alias df='df -x squashfs'
alias lsblk='lsblk -e1,7'
alias cb='xclip -selection clipboard'

source ~/.homesick/repos/homeshick/completions/homeshick-completion.bash

export EDITOR=nano
export SUDO_ASKPASS=ssh-askpass

if [ -f $HOME/.bashrc.local ]; then
    source $HOME/.bashrc.local
fi

