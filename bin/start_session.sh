#!/bin/sh
tmux new-session -d -s work
tmux new-window -n server
tmux new-window -n work
tmux new-window -n root
tmux select-window -t work:1
tmux -2 -u attach-session -t work
