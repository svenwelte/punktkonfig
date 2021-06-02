## Requirements

### fedora
```
dnf install git tig jq zsh htop lsd ansible nvim fzf fd-find ripgrep ranger
```

### homebrew
```
brew install zoxide git-delta git tig jq zsh htop lsd ansible nvim fzf fd ripgrep ranger reattach-to-user-namespace
```

### Pacman
```
pacman -S git tig jq zsh htop lsd ansible neovim fzf fd ripgrep ranger
```

## Setup

```
punktkonfig/setup.sh
exec zsh
punktkonfig/install.sh
```

## Install
```
# zsh
chsh --shell /bin/zsh

# iterm2 (only osx)
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/punktkonfig/iterm2"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

```

