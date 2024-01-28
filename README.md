`olenia` is an OCaml implementation of a basic version of [Lenia](https://en.wikipedia.org/wiki/Lenia), a non-discrete Game of Life, focused on real-time visualization.

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
As its main feature is real-time visualization, improving execution speed is important.
For now, it probably suffers from the excessive use of `Array.map` and `Array.init`. 
Neither flattening the map to a single array, nor initializing arrays outside of their functions (that get called a lot) helped speeding things up.
Allowing real-time modification of parameters would also be nice.


```sh
$ ./olenia --help
olenia [-s int] [-w int] [-h int] [-mu float] [-sd float] [-kmu float] [-ksd float] [-radius int] [-time float] [-scale int] [-shape <ellipse|square>] [-theme <borealis|redsoft|tournesol>]
  -s : map size (int)
  -w : window width (int)
  -h : window height (int)
  -time : time discretization (float)
  -mu : convolution gaussian mean (float)
  -sd : convolution gaussian standard deviation (float)
  -kmu : kernel gaussian mean (float)
  -ksd : kernel gaussian standard deviation (float)
  -radius : kernel radius (int)
  -scale : zoom level (int)
  -shape : cells shape (ellipse or square)
  -theme : color gradient (borealis, redsoft or tournesol)
```
