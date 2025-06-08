# -*- sh -*-
if [ -f /etc/profile ]; then
    source /etc/profile
fi

. $HOME/.bashrc

if [ -f "$HOME/.profile.local" ]; then
    . "$HOME/.profile.local"
fi
