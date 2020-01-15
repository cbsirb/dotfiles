#!/bin/sh

if [[ ! "$PATH" == *"/sbin"* ]]; then
  PATH="/sbin:$PATH"
fi

# if [[ ! "$PATH" == *"/usr/sbin"* ]]; then
#     export PATH="/usr/sbin:$PATH"
# fi

# Don't pollute path if not needed
if [[ ! "$PATH" == *"$HOME/.cargo/bin"* ]]; then
  PATH="$PATH:$HOME/.cargo/bin"
fi

# if [[ ! "$PATH" == *"$HOME/bin"* ]]; then
#   export PATH="$PATH:$HOME/bin"
# fi

if [[ ! "$PATH" == *"$HOME/.local/bin"* ]]; then
  PATH="$HOME/.local/bin:$PATH"
fi

# Use the ~/.local dir wherever possible
export C_INCLUDE_PATH=$HOME/.local/include
export CPLUS_INCLUDE_PATH=$HOME/.local/include
export LIBRARY_PATH=$HOME/.local/lib
export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig
export LD_LIBRARY_PATH=$HOME/.local/lib

if [[ ! "$(manpath -q)" == *"$HOME/.local/share/man"* ]]; then
  export MANPATH=$HOME/.local/share/man:$(manpath -q)
fi

export PATH
