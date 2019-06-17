## Requirements

### Fedora
```
dnf install git tig jq zsh htop lsd ansible nvim vim
```

## Setup

```
mkdir -p ~/tmp
mkdir -p ~/bin
mkdir -p ~/.vim/bundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
git clone git@github.com:svenwelte/punktkonfig.git ~/punktkonfig
(cd ~/punktkonfig && stow git && stow vim && stow zsh && stow tmux)
ln -s ~/punktkonfig/git/.gitignore

```

## Install
```
# zsh
chsh --shell /bin/zsh
exec zsh

# vim
nvim +PlugInstall +qall

# iterm2 (only osx)
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/punktkonfig/iterm2"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

```

