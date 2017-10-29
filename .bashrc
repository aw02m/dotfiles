# some more ls aliases
alias ls='ls -CF --color=auto'
alias la='ls -AlFh --show-control-chars --color=auto'
alias shutdown='shutdown -h now'
alias relogin='exec $SHELL -l'
alias fm='ranger'
alias vi='vim'
alias emacs='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'

export PATH=$PATH:$HOME/bin/
export PS1="\[\033[1;33m\]\u\[\033[1;32m\]@\[\033[4;35m\]\h\[\033[0m\]\[\033[1;32m\]:\[\033[1;36m\]\W \[\033[0;37m\](\D{%m/%d/%Y} \t hist:\#)\[\033[0m\]\n\[\033[1;32m\]\$ \[\033[0m\]"
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

# ranger settings
ranger() { [ -n "$RANGER_LEVEL" ] && exit || LESS="$LESS -+F -+X" command ranger "$@"; }
[ -n "$RANGER_LEVEL" ] && PS1="(RANGER) $PS1"
