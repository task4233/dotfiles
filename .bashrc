# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# history setting 
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# prompt setting
shopt -s checkwinsize
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac
force_color_prompt=yes
if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='\[\e[m\]$(pwd)\[\e[m\] \[\e[1;36m\]$(__git_ps1 "(%s)")\n\[\e[m\]\$ '
else
    PS1='\w\$ '
fi

# aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# bash completion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
source ~/.git-completion.bash
source ~/.git-prompt.sh

# set path
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export GOPATH=$HOME/work/go
export GOROOT=$HOME/go
export PATH=$GOPATH/bin:$PATH

# tmux setting
SESSION_NAME=term

if [[ -z "$TMUX" && -z "$STY" ]] && type tmux >/dev/null 2>&1; then
  option=""
  if tmux has-session -t ${SESSION_NAME}; then
    option="attach -t ${SESSION_NAME}"
  else
    option="new -s ${SESSION_NAME}"
  fi  
  tmux $option \; rename-window "editor" \; new-window \; rename-window "cmd" \; next-window && exit
fi
