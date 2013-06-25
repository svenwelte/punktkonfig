#!/bin/sh
tmux new-session -d -s $1
tmux rename-window -t $1:1 'editor'
tmux new-window -t $1:2 -n 'server'
tmux new-window -t $1:3 -n 'misc'
tmux new-window -t $1:4 -n 'work'
tmux new-window -t $1:5 -n 'root'
tmux select-window -t $1:1


if [ "$1" = "clj" ]; then
  tmux send-keys -t $1:1 "emacs -nw" C-m
  tmux send-keys -t $1:2 "lein repl" C-m
  tmux send-keys -t $1:3 "bundle exec compass watch" C-m
  tmux send-keys -t $1:4 "git status" C-m
  tmux send-keys -t $1:5 "sudo bash" C-m
fi

if [ "$1" = "java" ]; then
  tmux send-keys -t $1:1 "vim" C-m
  #tmux send-keys -t $1:2 "" C-m
  tmux send-keys -t $1:3 "bundle exec compass watch" C-m
  tmux send-keys -t $1:4 "git status" C-m
  tmux send-keys -t $1:5 "sudo bash" C-m
fi

tmux -2 -u attach-session -t $1
