export PATH=./bin:~/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
#export TERM="xterm-256color"
export TERM="screen-256color"

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
source ~/punktkonfig/zsh/cdargs-zsh.sh

zstyle ':completion:*' menu select


alias -r gst="git st"
alias -r gs="git st"
alias -r h="history | grep "
alias -r pd="popd"
alias -r dirs="dirs -v"
alias -r l="ls -la"


# do not autocorrect at all
unsetopt correct_all

# do not share histories
unsetopt SHARE_HISTORY
# only append history entries
setopt INC_APPEND_HISTORY
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

# do not beep
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
