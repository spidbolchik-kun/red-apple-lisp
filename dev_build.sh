#!/usr/bin/bash


set -e
eval RA_PATH=$(cat library-path)
[ -d "$RA_PATH" ] || ln -s "$(realpath stdlib)" "$(realpath "$RA_PATH")"
gsc -prelude "(define library-path \"$RA_PATH\")" mspm.scm
rm -f stdlib/mspm.o1
mv mspm.o1 stdlib
gsc -exe -prelude "(define library-path \"$RA_PATH\")" -o ra ra.scm
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin
rm -f ~/.local/bin/ra
mv ra ~/.local/bin/
ln -s "$(realpath runtime.scm)" "$(realpath stdlib/runtime.scm)"
