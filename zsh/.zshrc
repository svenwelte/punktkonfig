#zmodload zsh/zprof
export PATH=./bin:~/bin:~/punktkonfig/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:node_modules/.bin
source "$HOME/.zplugin/bin/zplugin.zsh"

autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

zplugin ice wait"0" blockf silent
zplugin light zsh-users/zsh-completions

zplugin ice wait'0' atload'_zsh_autosuggest_start' silent;
zplugin light zsh-users/zsh-autosuggestions

zplugin ice wait"0" atinit"zpcompinit; zpcdreplay" silent
zplugin light zdharma/fast-syntax-highlighting

#zplugin light superbrothers/zsh-kubectl-prompt
#zplugin light ahmetb/kubectx

zplugin ice wait"0" silent
zplugin light zdharma/history-search-multi-word

zplugin ice wait"0" silent
zplugin light hlissner/zsh-autopair

_fzf_compgen_path() {
  echo "$1"
  command fd --type f --hidden --follow --exclude ".git" "$1"
}

_fzf_compgen_dir() {
  command fd --type d --hidden --follow --exclude ".git"  "$1"
}

export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND='fd --type f'
zplugin snippet https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh
zplugin snippet https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh

export NVM_LAZY_LOAD=true
zplugin ice wait'1' silent;
zplugin light lukechilds/zsh-nvm

autoload -Uz compinit
compinit
zplugin cdreplay -q

autoload -U colors
colors

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr '%F{28}●'
zstyle ':vcs_info:*' unstagedstr '%F{11}●'

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git
precmd () {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}]'
    } else {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{blue}]'
    }

    vcs_info
}

setopt prompt_subst
PROMPT='%F{blue}%B%~${vcs_info_msg_0_}%F{blue} %(?/%F{blue}/%F{red})$ %F{reset}%b'
#RPROMPT='%{$fg[magenta]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}'

unsetopt menucomplete
zstyle ':completion:*' menu select
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==02=01}:${(s.:.)LS_COLORS}")';

alias -r m="cmatrix -bas"
alias -r mc="mc --nosubshell"
alias -r gst="git st"
alias -r gs="git st"
alias -r h="history | grep "
alias -r pd="popd"
alias -r dirs="dirs -v"
alias -r sl="ls"
alias -r b="bundle exec"
alias -r br="bundle exec rspec"
alias -r r="bundle exec rspec"
alias -r df="df -h"
alias -r du="du -h"


# do not autocorrect at all
unsetopt correct_all

#
# HISTORY
#

# do not share histories
unsetopt SHARE_HISTORY
# only append history entries
setopt INC_APPEND_HISTORY
setopt hist_ignore_dups
setopt hist_ignore_space
HISTSIZE=1000
SAVEHIST=1000
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

bindkey -e
export EDITOR=vim

#
# Temp Dir Stuff
#
export TMP="$HOME/tmp"
export TEMP="$TMP"
export TMPDIR="$TMP"

if [ ! -d "${TMP}" ]; then mkdir "${TMP}"; fi

source ~/.profile

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#zprof
