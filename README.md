## Basic Setup

```
for f in ackrc emacs.d gitconfig gitignore gvimrc  pryrc tmux.conf vimrc Xdefaults zsh zshrc
do
  ln -s "punktkonfig/$f"  ".$f"
done
mkdir -p ~/.vim/bundle
mkdir -p ~/tmp
mkdir -p ~/bin
ln -s punktkonfig/bin/start_session.sh ~/bin/start_session.sh
git clone http://github.com/gmarik/vundle.git  ~/.vim/bundle/vundle
```
