#!/bin/sh 
set -eu 

opam init --bare --no-setup -q 

gclone () { 	
	git clone "$1" 	
	(cd "$2" || exit 1 	
	git checkout "$3") 
} 

gclone https://github.com/gabrielhdt/lambdapi.git lambdapi aae26f2d 
gclone https://github.com/SRI-CSL/PVS.git PVS pvs7.1 

(cd lambdapi || exit 1  
opam switch create . --locked --deps-only --yes  
eval "$(opam env)"  
make install) 

(cd personoj || exit 1  
bmake -C encoding install) 
(cd PVS || exit 1  
for p in $(find translations/prelude_patches/ -name '*.diff' | sort); do 	 
	patch lib/prelude.pvs "${p}"  
done  
./build.sh)


