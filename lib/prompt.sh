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
    if [[ "$(basename "$HOME")" != $(whoami) ]] || [[ -n "$HOSTNAME_PS" ]]; then
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
