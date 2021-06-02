#/bin/bash


cd $HOME

#
# asdf
#
asdf plugin add java
asdf plugin add nodejs
asdf plugin add direnv
asdf plugin add terraform
asdf install


#
# vim
#
nvim +PlugInstall +qall 

#
# tmux
#
~/.tmux/plugins/tpm/bin/install_plugins
