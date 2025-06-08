# COMMANDS

function version_at_least() {
    [[ "$(printf '%s\n' "$@" | sort -V | head -n 1)" == "$1" ]]
}

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

function is_host_pingable() {
    timeout "${2:-0.2}" ping -q -n -c 1 "$1" 2> /dev/null > /dev/null
}

function link_to_file() {
    local f
    local OPTIND=
    local had_error=false
    local verbose=false
    local actually_do=true
    local dry_run=false
    while getopts "vn" opt; do
        case "$opt" in
            v)
                verbose=true
                ;;
            n)
                actually_do=false
                dry_run=true
                ;;
            \?)
                echo "Invalid option: -$opt"
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))
    for f in "$@"; do
        if [ -L "$f" ]; then
            local target=$(realpath "$f")
            if $verbose || $dry_run; then
                echo "Copying $target to $f"
            fi
            if $actually_do; then
                cp --remove-destination "$target" "$f"
            fi
        else
            echo "Error: $f is not a symlink."
            had_error=true
            continue
        fi
    done
    if $had_error; then
        echo "Error occurred."
        return 1
    fi
}
