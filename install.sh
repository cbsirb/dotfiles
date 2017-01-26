#!/usr/bin/bash

set -e

# Create the backup directory
if [ ! -d ".bak" ]; then
    mkdir .bak
fi

function backup_file {
    if [ ! -h "$1" ] && [ -f "$1" ]; then
        mv "$1" .bak/
    elif [ -h "$1" ]; then
        rm "$1"
    fi
}

function backup_dir {
    if [ ! -h "$1" ] && [ -d "$1" ]; then
        mv "$1" .bak/
    elif [ -h "$1" ]; then
        rm "$1"
    fi
}

function install_dotfile {
    echo Installing file $1
    backup_file "$HOME/.$1"
    ln -s "$(realpath $1)" "$HOME/.$1"
}

function install_dotdir {
    echo Installing directory $1
    backup_dir "$HOME/.$1"
    ln -s "$(realpath $1)" "$HOME/.$1"
}

function install_config_dir {
    echo Installing $1
    backup_dir "$HOME/.config/$1"
    ln -s "$(realpath $1)" "$HOME/.config/$1"
}

install_dotfile "bash_aliases"
install_dotfile "bash_env"
install_dotfile "bash_functions"
install_dotfile "bash_init"
install_dotfile "bash_logout"
install_dotfile "bash_profile"
install_dotfile "bashrc"

install_dotfile "tmux.conf"

backup_dir "$HOME/.tmux.d"
mkdir "$HOME/.tmux.d"
git clone https://github.com/tmux-plugins/tmux-yank "$HOME/.tmux.d/tmux-yank"

install_dotfile "Xresources"

install_dotdir "vim"
install_dotfile "vimrc"

install_dotdir "emacs.d"

if [ ! -d "$HOME/.config" ]; then
    mkdir "$HOME/.config"
fi

install_config_dir "i3"
install_config_dir "conky"
install_config_dir "mpd"
install_config_dir "ncmpcpp"

