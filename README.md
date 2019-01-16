# Source Code for [https://r-o-b.github.io/little-differ](https://r-o-b.github.io/little-differ)

All of the Elm code lives in `diff.elm` and relies on the [elm-lang/html][html] library. 

[html]: http://package.elm-lang.org/packages/elm-lang/html/latest 


## Build Instructions

Run the following commands from the root of this project:

```bash
bower install
elm-make diff.elm --output=elm.js
```

Then open `index.html` in your browser!
