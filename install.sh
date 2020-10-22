#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

# Create the backup directory
if [ ! -d ".bak" ]; then
    mkdir .bak
fi

backup_file() {
    if [ ! -h "$1" ] && [ -f "$1" ]; then
        mv "$1" .bak/
    elif [ -h "$1" ]; then
        rm "$1"
    fi
}

backup_dir() {
    if [ ! -h "$1" ] && [ -d "$1" ]; then
        if [ -d ".bak/$(basename $1)" ]; then
            echo "Warning: removing old backup for $(basename $1)"
            rm -rf ".bak/$(basename $1)"
        fi
        mv "$1" .bak/
    elif [ -h "$1" ]; then
        rm -r "$1"
    fi
}

install_executable() {
    echo Installing file $1
    backup_file "$HOME/.local/bin/$(basename $1)"
    ln -s "$(realpath $1)" "$HOME/.local/bin/$(basename $1)"
}

install_dotfile() {
    echo Installing file $1
    backup_file "$HOME/.$(basename $1)"
    ln -s "$(realpath $1)" "$HOME/.$(basename $1)"
}

install_dotdir() {
    echo Installing directory $1
    backup_dir "$HOME/.$(basename $1)"
    ln -s "$(realpath $1)" "$HOME/.$(basename $1)"
}

install_config_dir() {
    echo Installing $1
    backup_dir "$HOME/.config/$(basename $1)"
    ln -s "$(realpath $1)" "$HOME/.config/$(basename $1)"
}

if [ ! -d "$HOME/.local" ]; then
    mkdir "$HOME/.local"
fi

if [ ! -d "$HOME/.local/bin" ]; then
    mkdir "$HOME/.local/bin"
fi

install_dotfile "bash_aliases"
install_dotfile "bash_env"
install_dotfile "bash_functions"
install_dotfile "bash_logout"
install_dotfile "bash_profile"
install_dotfile "bashrc"
install_dotfile "inputrc"
install_dotfile "zshrc"
install_dotfile "zprofile"
install_dotfile "zpreztorc"
install_dotfile "p10k.zsh"

install_dotfile "gitconfig"

install_dotfile "tmux.conf"

install_dotfile "gdbinit"

install_dotfile "Xresources"

install_dotdir "vim"
install_dotfile "vimrc"
install_dotfile "gvimrc"

install_dotdir "emacs.d"

if [ ! -d "$HOME/.config" ]; then
    mkdir "$HOME/.config"
fi

for xfile in bin/*
do
    install_executable $xfile
done
