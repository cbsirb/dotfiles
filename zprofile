#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# nix
#
if [ -e /home/csirb/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/csirb/.nix-profile/etc/profile.d/nix.sh;

  # export XDG_DATA_DIRS=$XDG_DATA_DIRS:$HOME/.nix-profile/share
fi

#
# Browser
#

export BROWSER='x-www-browser'

#
# Editors
#

export GIT_EDITOR='vim'
export EDITOR='vim'
export VISUAL='emacs'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  /usr/local/{bin,sbin}
  ~/.local/bin
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# Enable for proper wayland support
export USE_WAYLAND=1

if [[ -n "$USE_WAYLAND" ]]; then
    export _JAVA_AWT_WM_NONREPARENTING=1
    export MOZ_ENABLE_WAYLAND=1
    export QT_QPA_PLATFORM=wayland
    export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
    export QT_QPA_PLATFORMTHEME=qt5ct
    export XDG_CURRENT_DESKTOP=sway
else
    # Disable this on wayland, causes jagged fonts on GTK apps
    export GTK_USE_PORTAL=1
fi

#
# Other stuff
#

# export LIBVA_DRIVER_NAME=iHD
export LIBVA_DRIVER_NAME=i965

export PATH="$HOME/.cargo/bin:$PATH"
