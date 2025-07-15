set -e
eval RA_PATH=$(cat library-path)
mkdir -p "$RA_PATH"
gsc -prelude "(define library-path \"$RA_PATH\")" mspm.scm
mv mspm.o1 "$RA_PATH"
gsc -exe -prelude "(define library-path \"$RA_PATH\")" -o ra ra.scm
mkdir ~/.local/bin
mv ra ~/.local/bin/
cp runtime.scm "$RA_PATH"
cp stdlib/builtins.ra "$RA_PATH"
