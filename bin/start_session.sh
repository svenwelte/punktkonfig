#!/bin/sh

if [ -z "$1" ]; then
  echo "\nusage start_session.sh [session]\n"
  exit -1
fi

tmux new-session -d -s $1
tmux rename-window -t $1:1 'editor'
tmux new-window -t $1:2 -n 'server'
tmux new-window -t $1:3 -n 'misc'
tmux new-window -t $1:4 -n 'work'
tmux new-window -t $1:5 -n 'root'
tmux select-window -t $1:1

tmux -2 -u attach-session -t $1
