  $ echo 'symbol true: foo bar baz;' | ../autosolve.exe --fixed
  symbol true : foo bar baz ≔
  begin
    why3;
  end;

  $ echo 'symbol foo: TYPE; symbol bar: TYPE;' | ../autosolve.exe --fixed
  symbol foo : TYPE ≔
  begin
    why3;
  end;
  symbol bar : TYPE ≔
  begin
    why3;
  end;
