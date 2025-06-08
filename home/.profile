# -*- sh -*-
if [ -f /etc/profile ]; then
    source /etc/profile
fi

READLINK=readlink
if command -v greadlink > /dev/null; then
    READLINK=greadlink
fi

if command -v java > /dev/null; then
    export JAVA_HOME="$(dirname $(dirname $($READLINK -f $(which java))) | sed 's@/jre$@@')"
fi

. $HOME/.bashrc

source "$HOME/.homesick/repos/homeshick/homeshick.sh"

HOME_CONSOLE_REPO_HOME=`$READLINK -f "$HOME/.bashrc"`
HOME_CONSOLE_REPO_HOME=`dirname "$HOME_CONSOLE_REPO_HOME"`
export HOME_CONSOLE_REPO=`dirname "$HOME_CONSOLE_REPO_HOME"`

source "$HOME_CONSOLE_REPO/lib.sh"

if [ -n "$BASH_VERSION" ]; then
    source "$HOME_CONSOLE_REPO/jumptorc"
fi

export ELECTRON_TRASH="trash-cli"
# export GTK_OVERLAY_SCROLLING=0

source "$HOME_CONSOLE_REPO/pathrc"

if command -v python3 > /dev/null 2> /dev/null; then
    ANY_PYTHON_CMD=python3
else
    ANY_PYTHON_CMD=python
fi

export NPROC="$($ANY_PYTHON_CMD -c 'import multiprocessing; print(multiprocessing.cpu_count())' 2> /dev/null)"
if [ "$NPROC" ]; then
    PARALLEL_LEVEL="$[ NPROC * 7 / 8 ]"
    export PARALLEL_LEVEL="$[ $PARALLEL_LEVEL < 40 ? $PARALLEL_LEVEL : 40 ]"
    export MAKEFLAGS="--jobs=$PARALLEL_LEVEL --load-average=$[ NPROC * 3 / 4 ]"
    export CMAKE_BUILD_PARALLEL_LEVEL="$PARALLEL_LEVEL"
else
    unset NPROC
fi

if command -v ccache > /dev/null 2> /dev/null; then
    export CMAKE_C_COMPILER_LAUNCHER="ccache"
    export CMAKE_CXX_COMPILER_LAUNCHER="ccache"
    export CMAKE_CUDA_COMPILER_LAUNCHER="ccache"
fi

function has_host() {
    timeout "${2:-0.2}" ping -q -n -c 1 "$1" 2> /dev/null > /dev/null
}

if [ -f "$HOME/.profile.local" ]; then
    . "$HOME/.profile.local"
fi

. $HOME/.bashrc