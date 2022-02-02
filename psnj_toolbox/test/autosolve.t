  $ echo 'symbol true : foo bar baz;' | psnj autosolve --fixed
  symbol true : foo bar baz ≔
  begin
  why3;
  end;

  $ echo 'symbol foo: TYPE; symbol bar: TYPE;' | psnj autosolve --fixed
  symbol foo : TYPE ≔
  begin
  why3;
  end;
  symbol bar : TYPE ≔
  begin
  why3;
  end;

