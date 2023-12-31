## Requirements

### fedora
```
dnf install git tig jq zsh htop lsd ansible nvim fzf fd-find ripgrep ranger stow ctop
```

### homebrew
```
brew install zoxide git-delta git tig jq zsh htop lsd ansible nvim fzf fd ripgrep ranger reattach-to-user-namespace stow ctop
```

### Pacman
```
pacman -S git tig jq zsh htop lsd ansible neovim fzf fd ripgrep ranger stow ctop
ansible-galaxy install kewlfft.aur
yay zoxide
```

## Setup

```
curl -s https://raw.githubusercontent.com/svenwelte/punktkonfig/master/setup.sh | bash /dev/stdin
tmux
zsh
punktkonfig/install.sh
```

## Install
```
# zsh
chsh --shell /bin/zsh

# iterm2 (only osx)
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/punktkonfig/iterm2"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

# Note: this is needed for ideavim keyrepeat
defaults write -g ApplePressAndHoldEnabled -bool false
# delay 180ms
defaults write -g InitialKeyRepeat -int 12
# key repeat 30ms
defaults write -g KeyRepeat -int 2

```
