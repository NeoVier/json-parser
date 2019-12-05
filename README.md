# simple-parser

This is a pretty rudimentary parser written completely in Haskell. For now, it can parse [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck) and [Json](https://en.wikipedia.org/wiki/JSON).
To run the parser, compile the whole thing with `ghc -o main Main.hs src/*` and run `./main [filename]`. The program will automatically recognize `.json` and `.bf` files and execute accordingly.

## Json
This was the initial motivation for this project, following [Tsoding's parser](https://github.com/tsoding/haskell-json), but as we need the same parser basics for other stuff, it's split into different modules. This also includes a pretty printing for Json objects and arrays.

## BrainFuck
Because BrainFuck isn't like Json as in it doesn't need to be encapsulated in an array or an object, the `brainFuckValue` parser returns a list of `BrainFuckValue`s. Besides the parser, I have implemented a [brainfuck interpreter](https://github.com/NeoVier/simple-parser/blob/master/src/BrainFuckInterpreter.hs), which, as expected, can run brainfuck code.

