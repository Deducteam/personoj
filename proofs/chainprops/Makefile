all: psnj-chainprops

_build/default/chainprops.exe: chainprops.ml
	@dune build

psnj-chainprops: _build/default/chainprops.exe
	@cp -f _build/default/chainprops.exe psnj-chainprops
