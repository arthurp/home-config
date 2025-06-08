function version_at_least() {
    [[ "$(printf '%s\n' "$@" | sort -V | head -n 1)" == "$1" ]]
}
