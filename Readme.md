#   pPEG in Haskell

This is work in progress. My first Haskell project.

pPEG.hs is a single module with no dependencies (other std than import DATA).

Fully functional except for extensions, which are not yet implemented.

The API has a Peg.compile that takes a pPEG grammar and returns a parser function (or an error that can be printed). See Json.hs for example.




