#!/usr/bin/bash

set -e
eval RA_PATH=$(cat library-path)
[ -d "$RA_PATH" ] || mkdir -p "$RA_PATH"
gsc -prelude "(define library-path \"$RA_PATH\")" mspm.scm
mv mspm.o1 "$RA_PATH"
gsc -exe -prelude "(define library-path \"$RA_PATH\")" -o ra ra.scm
[ -d ~/.local/bin ] || mkdir ~/.local/bin
mv ra ~/.local/bin/
cp runtime.scm "$RA_PATH"
cp stdlib/builtins.ra "$RA_PATH"
