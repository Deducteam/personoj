  $ ../chainprops.exe example.dep < example.lp
  symbol tgt: @imp H0 (@imp H1 P);
  symbol hyp0: H0;
  symbol hyp1: H1;


  $ ../chainprops.exe foo.dep < foo.lp
  symbol foo: @imp Bar (@imp Baz Foo);
  symbol bar: @imp Frob (@imp Nitz (@imp Baz Bar));
  symbol baz: Baz;
  symbol frob: Frob;
  symbol nitz: Nitz;
