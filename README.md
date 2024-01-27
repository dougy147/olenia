`olenia` is an OCaml implementation of [Lenia](https://en.wikipedia.org/wiki/Lenia), a non-discrete Game of Life.

# Quick launch

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o olenia -linkpkg -package graphics olenia.ml
$ ./olenia
```

| Key      | Functionality                              |
|----------|--------------------------------------------|
| `r`      | Load and start a new random world          |
| `i`      | Toggle informations                        |
| `<Esc>`  | Close                                      |


<p align="center">
<img src="https://raw.githubusercontent.com/dougy147/olenia/master/sample.gif" width="75%" />
</p>

Despite using FFT, `olenia` is slow: 16fps on 64x64 maps, and only 2fps on 128x128.
However, as its main feature is real-time visualization, it is important to improve execution speed.
For now, it probably suffers from the excessive use of `Array.map` and `Array.init`. 
Neither flattening the map to a single array, nor allocating memory for the arrays outside of the functions helps speeding things up.
