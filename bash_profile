#!/bin/bash

[[ -f ~/.profile ]] && . ~/.profile

if [[ $- == *i* ]]; then
  [[ -f ~/.bashrc ]] && . ~/.bashrc
fi
