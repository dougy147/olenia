`olenia` is an OCaml implementation of [Lenia](https://en.wikipedia.org/wiki/Lenia), a non-discrete Game of Life.

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o olenia -linkpkg -package graphics olenia.ml
$ ./olenia
```

| Key      | Functionality                              |
|----------|--------------------------------------------|
| `r`      | Load and start a new random world          |
| `<Esc>`  | Close                                      |


<p align="center">
<img src="https://raw.githubusercontent.com/dougy147/olenia/master/sample.gif" width="80%" />
</p>
