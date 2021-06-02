### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
        print -P "%F{160}▓▒░ The clone has failed.%f"
fi
source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit installer's chunk

eval "$(starship init zsh)"
eval "$(zoxide init zsh)"

export PATH=./bin:~/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:node_modules/.bin

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export ASDF_DIR="$HOME/.asdf"
[[ -s "$ASDF_DIR/asdf.sh" ]] && source "$ASDF_DIR/asdf.sh"
fpath=(${ASDF_DIR}/completions $fpath)

eval "$(asdf exec direnv hook zsh)"

_fzf_compgen_path() {
  command rg --type f "$1"
}

_fzf_compgen_dir() {
  command fd --type d --hidden --follow --exclude ".git"  "$1"
}

export FZF_DEFAULT_COMMAND='rg --files'
export FZF_CTRL_T_COMMAND='rg --files'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#
# Load Plugins
#
zinit light-mode wait lucid atload'_zsh_autosuggest_start' for \
  zsh-users/zsh-autosuggestions

zinit light-mode wait lucid atinit'zicompinit; zicdreplay' for \
  zdharma/fast-syntax-highlighting

zinit light-mode wait lucid for \
  zsh-users/zsh-completions \
  zdharma/history-search-multi-word \
  hlissner/zsh-autopair

autoload -Uz colors
colors

function vi-accept-line() {
  VI_KEYMAP=main
  zle accept-line
}

zle -N vi-accept-line

bindkey -v
#bindkey -e
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
export KEYTIMEOUT=1


# unsetopt menucomplete
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' format 'Completing: %d'
zstyle ':completion:*' format '%B---- %d%b'
zstyle ':completion:*:descriptions' format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format "%B$fg[red]%}---- no match for: $fg[white]%d%b"
zstyle ':completion:*' group-name ''
zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:approximate:*' max-errors 2


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
#bindkey "^[[A" history-beginning-search-backward
#bindkey "^[[B" history-beginning-search-forward

# update word boundaries
WORDCHARS=${WOARCHARS:s/-=_//}

export EDITOR=vim

source ~/.profile

