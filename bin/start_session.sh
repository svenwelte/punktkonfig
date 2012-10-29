#!/bin/sh
tmux new-session -d -s work
tmux rename-window -t work:1 'editor'
tmux new-window -t work:2 -n 'server'
tmux new-window -t work:3 -n 'misc'
tmux new-window -t work:4 -n 'work'
tmux new-window -t work:5 -n 'root'
tmux select-window -t work:1


if [ "$1" = "clj" ]; then
  tmux send-keys -t work:1 "emacs -nw" C-m
  tmux send-keys -t work:2 "lein repl" C-m
  tmux send-keys -t work:3 "bundle exec compass watch" C-m
  tmux send-keys -t work:4 "git status" C-m
  tmux send-keys -t work:5 "sudo bash" C-m
fi

if [ "$1" = "java" ]; then
  tmux send-keys -t work:1 "vim" C-m
  #tmux send-keys -t work:2 "" C-m
  tmux send-keys -t work:3 "bundle exec compass watch" C-m
  tmux send-keys -t work:4 "git status" C-m
  tmux send-keys -t work:5 "sudo bash" C-m
fi

tmux -2 -u attach-session -t work
