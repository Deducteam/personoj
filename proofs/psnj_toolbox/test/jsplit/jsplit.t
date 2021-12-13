  $ psnj jsplit -p ff < input.json; find . -type f -printf "\n%p\n" -exec cat {} \;
  
  ./ff_bar
  { "name": "bar", "attr": "frobz" }
  
  ./ff_foo
  { "name": "foo", "attr": "baz"   }
