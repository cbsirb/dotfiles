#!/bin/bash

[[ -f ~/.profile ]] && . ~/.profile

[[ -f ~/.env_private ]] && . ~/.env_private

if [[ $- == *i* ]]; then
  [[ -f ~/.bashrc ]] && . ~/.bashrc
fi

if [ -e /home/csirb/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/csirb/.nix-profile/etc/profile.d/nix.sh;
fi

export PATH="$HOME/.cargo/bin:$PATH"
