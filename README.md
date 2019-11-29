# simple-parser

This is a pretty rudimentary parser written completely in Haskell. For now, it can parse [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck) and [Json](https://en.wikipedia.org/wiki/JSON).

## Json Parser
This was the initial motivation for this project, following [Tsoding's parser](https://github.com/tsoding/haskell-json), but as we need the same parser basics for other stuff, it's split into different modules. This also includes a pretty printing for Json objects and arrays.

## BrainFuck Parser
Because BrainFuck isn't like Json as in it doesn't need to be encapsulated in an array or an object, the `brainFuckValue` parser returns a list of `BrainFuckValue`s. Comments are not accepted (yet).
