export PATH=./bin:~/bin:~/punktkonfig/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:node_modules/.bin

autoload -Uz compinit
compinit

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

unsetopt menucomplete
zstyle ':completion:*' menu select
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==02=01}:${(s.:.)LS_COLORS}")';

alias -r git="LANG=en_US.UTF-8 git"
alias -r gst="git st"
alias -r gs="git st"
alias -r h="history | grep "
alias -r pd="popd"
alias -r dirs="dirs -v"
alias -r l="ls -lah"
alias -r ll="ls -lah"
alias -r sl="ls"
alias -r b="bundle exec"
alias -r br="bundle exec rspec"
alias -r r="bundle exec rspec"
alias -r df="df -h"
alias -r du="du -h"
alias -r gradle="./gradlew"


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

# into X11 clipboard
x() { echo $1 | xclip -selection c. }

zmodload zsh/regex
export CLICOLOR=1
if [[ `uname -a` -regex-match "Linux" ]]; then
  alias -r ls="ls -la --color=auto"
fi

# update word boundaries
WORDCHARS=${WOARCHARS:s/-=_//}

bindkey -e
export EDITOR=vim

ptop() {
  watch -n 1 'psql90 -U postgres -c "SELECT current_query FROM pg_stat_activity"'
}


#
# Temp Dir Stuff
#
export TMP="$HOME/tmp"
export TEMP="$TMP"
export TMPDIR="$TMP"

if [ ! -d "${TMP}" ]; then mkdir "${TMP}"; fi

source ~/.profile

nvm() {
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
  nvm "$@"
}

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
