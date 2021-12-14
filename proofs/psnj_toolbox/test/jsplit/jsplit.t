  $ psnj jsplit -p ff < input.json
  ff_bar.json
  ff_foo.json

  $ find . -type f -printf "\n%p\n" -exec cat {} \;
  
  ./ff_foo.json
  { "name": "foo", "attr": "baz"   }
  
  ./ff_bar.json
  { "name": "bar", "attr": "frobz" }
