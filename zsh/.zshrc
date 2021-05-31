#typeset -g ZPLG_MOD_DEBUG=1

export PATH=./bin:~/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:node_modules/.bin

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

export ASDF_DIR="$HOME/.asdf"
[[ -s "$ASDF_DIR/asdf.sh" ]] && source "$ASDF_DIR/asdf.sh"
fpath=(${ASDF_DIR}/completions $fpath)

_fzf_compgen_path() {
  command rg --type f "$1"
}

_fzf_compgen_dir() {
  command fd --type d --hidden --follow --exclude ".git"  "$1"
}

export FZF_DEFAULT_COMMAND='rg --files'
export FZF_CTRL_T_COMMAND='rg --files'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

module_path+=( "/Users/svenwelte/.zinit/bin/zmodules/Src" )
zmodload zdharma/zplugin

#
# Setup zinit
#
if [ ! -d "${HOME}/.zinit" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"
fi
source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

#
# Load Plugins
#
zinit light-mode wait lucid atload'_zsh_autosuggest_start' for \
  zsh-users/zsh-autosuggestions

zinit light-mode wait lucid for \
  zsh-users/zsh-completions \
  zdharma/history-search-multi-word \
  hlissner/zsh-autopair

zinit light-mode wait lucid atinit'zicompinit; zicdreplay' for \
  zdharma/fast-syntax-highlighting

#zinit light superbrothers/zsh-kubectl-prompt
#zinit light ahmetb/kubectx


autoload -Uz colors 
colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' stagedstr "%{$fg[green]%}●%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "%{$fg[yellow]%}●%{$reset_color%}"
zstyle ':vcs_info:*' branchformat "%{$fg[green]%}%b%{$reset_color%}"
#zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
precmd () {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats " [%{$fg[green]%}%b%{$reset_color%}%c%u]"
    } else {
        zstyle ':vcs_info:*' formats " [%{$fg[green]%}%b%{$reset_color%}%c%u%{$fg[red]%}●%{$reset_color%}]"
    }

    vcs_info
}


function zle-keymap-select() {
  zle reset-prompt
  zle -R
}

zle -N zle-keymap-select

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

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[green]%} [% NORMAL]% %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}$EPS1"
    zle reset-prompt
}

setopt prompt_subst
PROMPT='%F{blue}%B%~${vcs_info_msg_0_}%F{blue} %(?/%F{blue}/%F{red})$ %F{reset}%b'
#RPROMPT='%{$fg[magenta]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}'

unsetopt menucomplete
zstyle ':completion:*' menu select
#zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==02=01}:${(s.:.)LS_COLORS}")';
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"


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

export EDITOR=nvim

#
# Temp Dir Stuff
#
#export TMP="$HOME/tmp"
#export TEMP="$TMP"
#export TMPDIR="$TMP"

if [ ! -d "$HOME/tmp" ]; then mkdir "$HOME/tmp"; fi

source ~/.profile

#eval "$(nodenv init -)"

