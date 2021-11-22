  $ ../chainprops.exe example.lp example.dep
  symbol hyp1: H1;
  symbol hyp0: H0;
  symbol tgt: @imp H0 (@imp H1 P);


  $ ../chainprops.exe foo.lp foo.dep
  symbol nitz: Nitz;
  symbol frob: Frob;
  symbol baz: Baz;
  symbol bar: @imp Frob (@imp Nitz (@imp Baz Bar));
  symbol foo: @imp Bar (@imp Baz Foo);
