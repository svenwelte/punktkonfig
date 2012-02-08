export PATH=./bin:~/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
export TERM="xterm-256color"

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
source ~/.zsh/cdargs-zsh.sh


alias -r ls="ls -la --color=auto"
alias -r gst="git st"
alias -r gs="git st"

# do not autocorrect at all
unsetopt correct_all

# do not share histories
unsetopt SHARE_HISTORY

bindkey '^[1;5C' emacs-forward-word
bindkey '^[1;5D' emacs-backward-word
