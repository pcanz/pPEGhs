#   pPEG in Haskell

A proof of concept implementation of [pPEG] in Haskell.

My first Haskell project, lots of fun I like Haskell a lot.

pPEG.hs is a single module with no dependencies (other std lib DATA).

Fully functional pPEG except for extensions, which are not yet implemented.

The API has a Peg.compile that takes a pPEG grammar and returns a parser function (or an error that can be printed). 

See Json.hs for example.

I do not plan to maintain this repo, but it may be a good starting point for anyone implementing a similar PEG grammar-parser in Haskell.

