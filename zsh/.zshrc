[[ -r ~/.zsh/zsh-snap ]] || git clone --depth 1 -- https://github.com/marlonrichert/zsh-snap.git ~/.zsh/zsh-snap

source ~/.zsh/zsh-snap/znap.zsh
source ~/.zprofile

zstyle ':znap:*' repos-dir ~/.zsh

znap eval starship 'starship init zsh --print-full-init'
znap prompt

#znap source marlonrichert/zsh-autocomplete
ZSH_AUTOSUGGEST_STRATEGY=( history )
znap source zsh-users/zsh-autosuggestions

znap source zsh-users/zsh-syntax-highlighting
znap source hlissner/zsh-autopair
znap source zsh-users/zsh-history-substring-search
znap source joshskidmore/zsh-fzf-history-search


# zmodload zsh/complist

export PATH="$PATH:./bin:$HOME/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:node_modules/.bin"

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export ASDF_DIR="$HOME/.asdf"
[[ -s "$ASDF_DIR/asdf.sh" ]] && source "$ASDF_DIR/asdf.sh"
fpath=(${ASDF_DIR}/completions $fpath)

znap eval asdf "asdf exec direnv hook zsh"
znap eval zoxide "type zoxide > /dev/null && zoxide init zsh"

type aws_completer > /dev/null && complete -C aws_completer aws

bindkey -v
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
export KEYTIMEOUT=1

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

zstyle ':znap:*:*' git-maintenance off
zstyle ':completion:*' completer _expand _complete _list _oldlist
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'r:|[._-/]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true

alias -r awsume=". awsume"
alias -r m="cmatrix -bas"
alias -r mc="mc --nosubshell"
alias -r gst="git st"
alias -r h="history | grep "
alias -r pd="popd"
alias -r dirs="dirs -v"
alias -r df="df -h"
alias -r du="du -h"
alias -r vim=nvim
alias -r sl="ls"
alias -r ls=lsd
alias -r r=ranger
alias -r ll="ls -la"
alias -r l=ll

# do not autocorrect at all
unsetopt correct_all

#
# HISTORY
#

# only append history entries
setopt INC_APPEND_HISTORY
setopt hist_ignore_dups
setopt hist_ignore_space
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history

# you shall not beep
setopt NOBEEP

# automatically change into directories
setopt AUTO_CD

# auto pushd
setopt AUTO_PUSHD
DIRSTACKSIZE=20

# only complete the first common chars
setopt complete_in_word

bindkey '^[1;5C' emacs-forward-word
bindkey '^[1;5D' emacs-backward-word

bindkey "\e[H" beginning-of-line # Home
bindkey "\e[F" end-of-line # End

# update word boundaries
WORDCHARS=${WOARCHARS:s/-=_//}

export EDITOR=vim


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
