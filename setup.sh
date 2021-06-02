#!/bin/bash
cd $HOME

if [[ ! -d bin ]]; then
  echo "Create bin"
  mkdir bin
fi

if [[ ! -d tmp ]]; then
  echo "Create tmp"
  mkdir tmp
fi


if [[ ! -d .asdf ]]; then
  echo "Install asdf via git"
  git clone https://github.com/asdf-vm/asdf.git .asdf --branch v0.8.1
else
  echo "asdf already installed"
fi

if [[ ! -d punktkonfig ]]; then
  echo "Install punktkonfig via git"
  git clone git@github.com:svenwelte/punktkonfig.git punktkonfig
  (cd ~/punktkonfig && stow git && stow vim && stow zsh && stow tmux && stow idea && stow x11 && stow asdf)
  ln -s ~/punktkonfig/git/.gitignore
else
  echo "punkkonfig already installed"
fi

if [[ ! -d ./.tmux/plugins/tpm ]]; then
  echo "Install tmux plugin manager via git"
  git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm
else
  echo "tmux plugin manager already installed"
fi

if [[ ! -f ./.vim/autoload/plug.vim ]]; then
  echo "Install vim plugin manager"
  mkdir -p ~/.vim/autoload
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
else
  echo "vim plugin manager already installed"
fi

