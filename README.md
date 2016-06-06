# Source Code for [LittleDiffer.com](http://www.littlediffer.com/)

All of the Elm code lives in `diff.elm` and relies on the [elm-lang/html][html] library. 

[html]: http://package.elm-lang.org/packages/elm-lang/html/latest 


## Build Instructions

Run the following commands from the root of this project:

```bash
bower install
elm-make diff.elm --output index.html
```

Then open `index.html` in your browser!
