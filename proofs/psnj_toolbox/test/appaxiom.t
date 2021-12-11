  $ echo 'symbol true : imp P P;' | psnj appaxiom
  symbol true : @Prf (imp P P);

  $ echo 'symbol x : nat;' | psnj appaxiom --app 'El'
  symbol x : @El nat;
