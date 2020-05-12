#!/bin/bash

[[ -f ~/.profile ]] && . ~/.profile

if [[ $- == *i* ]]; then
  [[ -f ~/.bashrc ]] && . ~/.bashrc
fi

if [ -e /home/csirb/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/csirb/.nix-profile/etc/profile.d/nix.sh;
fi
