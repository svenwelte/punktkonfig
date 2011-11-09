# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="geoffgarside"

plugins=(git history-substring-search)

source $ZSH/oh-my-zsh.sh

export PATH=./bin:~/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin

if [ -f /opt/local/etc/profile.d/cdargs-bash.sh ]; then
  source /opt/local/etc/profile.d/cdargs-bash.sh
fi


alias root="sudo zsh"

# do not autocorrect at all
unsetopt correct_all

# do not share histories
unsetopt SHARE_HISTORY
