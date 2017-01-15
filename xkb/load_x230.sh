#!/bin/bash
set -x
xkbcomp -I$HOME/.xkb ~/.xkb/keymap/x230 $DISPLAY
