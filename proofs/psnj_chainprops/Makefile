all: psnj-chainprops

_build/default/psnj_chainprops.exe: psnj_chainprops.ml
	@dune build

psnj-chainprops: _build/default/psnj_chainprops.exe
	@cp -f _build/default/psnj_chainprops.exe psnj-chainprops
