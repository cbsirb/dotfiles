# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

HISTORY_IGNORE="(ls|pwd|exit|cd|ll|bg|fg|history)"

# Customize to your needs...
alias ls='ls -hAF --group-directories-first -v --color=auto'
alias tmux="tmux -2"
alias gdb='gdb -q'
alias cls='printf "\033c"'
alias fd='fd -HL'

# Own git
alias g='git'
alias gs='git status'
alias gl='git l -100'
alias gh='show_git_head'
alias gc='git checkout'
alias gcd='git checkout devel'
alias gcm='git checkout master'
alias gf='git flow'
alias gd='git diff'

# nix stuff
alias nix-update='nix-channel --update; nix-env -iA nixpkgs.nix'
alias nix-clean='nix-collect-garbage -d'
alias nixs='nix-env -qas'
alias nixi='nix-env -i'
alias nixe='nix-env -e'
alias nixu='nix-env -u'

# pacman/pikaur
alias paci="sudo pacman -S"
alias piki="pikaur -S"
alias pacI="sudo pacman -U"
alias pacx="sudo pacman -Rns"
alias pacq="pacman -Si"
alias pikq="pikaur -Si"
alias pacQ="pacman -Qi"
alias pacs="pacman -Ss"
alias piks="pikaur -Ss"
alias pacS="pacman -Qs"
alias pacl="pacman -Ql"
alias pacman-list-orphans="pacman --query --deps --unrequired"
alias pacman-clean='sudo pacman -Rns $(pacman --quiet --query --deps --unrequired)'
alias pacu="pikaur -Syu"

alias path='echo -e ${PATH//:/\\n}'

alias v='vim'

alias down-mp3='youtube-dl -x -f bestaudio --audio-format mp3 --audio-quality 0 --add-metadata --metadata-from-title "%(artist)s - %(title)s" -o "%(title)s.%(ext)s" -i'
alias down-list='youtube-dl -x -f bestaudio --audio-format mp3 --audio-quality 0 --add-metadata --metadata-from-title "%(artist)s - %(title)s" -o "%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" -i'
alias dlna-restart='sudo systemctl stop minidlna.service; sudo minidlnad -R; sleep 5; sudo pkill minidlnad; sudo systemctl start minidlna.service'

alias temps="paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/...$/.0°C/'"

# An alias won't work since we need parameters
mosh-tmux () {
    mosh "$@" -- tmux new -ADs mosh-session
}

mosh-screen() {
    mosh "$@" -- screen -DR mosh-session
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

if [ -n "$INSIDE_EMACS" ]; then
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi
