## Requirements

### fedora
```
dnf install git tig jq zsh htop lsd ansible nvim fzf fd-find ripgrep ranger
```

### homebrew
```
brew install git tig jq zsh htop lsd ansible nvim fzf fd ripgrep ranger reattach-to-user-namespace
```

### Pacman
```
pacman -S git tig jq zsh htop lsd ansible neovim fzf fd ripgrep ranger
```

## Setup

```
mkdir -p ~/tmp
mkdir -p ~/bin
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
git clone git@github.com:svenwelte/punktkonfig.git ~/punktkonfig
(cd ~/punktkonfig && stow git && stow vim && stow zsh && stow tmux && stow idea)
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

