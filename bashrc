# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
export HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# reedit a history substitution line if it failed
shopt -s histreedit
# edit a recalled history line before executing
shopt -s histverify

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# Ignore the ls command as well
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls'

_bash_history_sync() {
    # Append the command just entered to history file
    builtin history -a
    # Truncate file if needed
    HISTFILESIZE=$HISTSIZE
    # Clear the history of the running session
    builtin history -c
    # Reload the history
    builtin history -r
}

history() {                  #5
  _bash_history_sync
  builtin history "$@"
}

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    xterm-256color) color_prompt=yes;;
    screen-256color) color_prompt=yes;;
    eterm-color) color_prompt=yes;;
    rxvt-unicode-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# In case git-prompt.sh is missing
parse_git_branch() {
	git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PROMPT_DIRTRIM=3
PROMPT_COMMAND=_bash_history_sync

if [ "$color_prompt" = yes ]; then
    case "$TERM" in
        xterm*|rxvt*|tmux*|screen*|eterm*)
            if [ -f /usr/share/git/completion/git-prompt.sh ]; then
                source /usr/share/git/completion/git-prompt.sh

                export GIT_PS1_SHOWUPSTREAM=verbose
                export GIT_PS1_SHOWDIRTYSTATE=1
                export GIT_PS1_SHOWCOLORHINTS=1

                export PS1="\[\033[38;5;1m\]\u\[$(tput sgr0)\] @ \[$(tput sgr0)\]\[\033[38;5;12m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\]\[$(tput sgr0)\]\$(__git_ps1 ' (%s)')\[\033[38;5;1m\] >\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"
            else
                export PS1='\[\033[38;5;1m\]\u\[$(tput sgr0)\] @ \[$(tput sgr0)\]\[\033[38;5;12m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\$(parse_git_branch)\[\033[38;5;1m\] >\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]'
            fi
            ;;
        *)
            PS1='\u@\h:\w\$ '
            ;;
    esac
else
    PS1='\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

if [ -f ~/.bash_env ]; then
   . ~/.bash_env
fi

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# Functions definitions
if [ -f ~/.bash_functions ]; then
  . ~/.bash_functions
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


if [ -f ~/.bash_init ]; then
  source ~/.bash_init
fi

# typing '/etc' is the same as 'cd /etc'
shopt -s autocd

# small corrections to cd
shopt -s cdspell

# don't exit with running jobs (unless you force it)
shopt -s checkjobs

# same as cd but in more commands
shopt -s dirspell

# include . files in *
shopt -s dotglob

# fail if no glob matches
# shopt -s failglob

# ** pattern
shopt -s globstar

# [a-z] patterns
shopt -s globasciiranges

# pipe input to read
shopt -s lastpipe

# don't complete on empty
shopt -s no_empty_cmd_completion

# Qute characters that are needed
shopt -s complete_fullquote

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

PATH=$PATH:~/bin
