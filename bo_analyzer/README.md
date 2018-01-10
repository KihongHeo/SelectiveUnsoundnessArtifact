# Sparrow

Sparrow is a state-of-the-art static analyzer that aims to verify the absence
of fatal bugs in C source. Sparrow is designed by Abstract Interpretation and
the analysis is sound in design. Sparrow adopts a number of well-founded static
analysis techniques for scalability, precision, and user convenience.

## Build
To build Sparrow, you need
-   [OCaml][] >= 4.02.3
-   [Batteries][] >= 2.3.1
-   [Cil][] >= 1.7.3
-   [Ocamlgraph][] >= 1.8.6
-   [Apron][] >= 0.9.10
-   [Yojson][] >= 1.2.3

[Ocaml]: http://caml.inria.fr
[Batteries]: http://batteries.forge.ocamlcore.org
[Cil]: https://github.com/cil-project/cil
[Ocamlgraph]: http://ocamlgraph.lri.fr/index.en.html
[Apron]: http://apron.cri.ensmp.fr/library
[Yojson]: http://mjambon.com/yojson.html


The easiest way to install the prerequisites is to use [OPAM][]. For example:
```sh
$ opam batteries cil apron ocamlgraph yojson
```
[OPAM]: http://opam.ocamlpro.com/

Once you have installed all the prerequisites, run the build script:
```sh
$ ./build
```
Sparrow has been tested on Linux.

## Run the analysis
You can run Sparrow for buffer overflow detection on pre-processed C files. For example:
```sh
$ ./main.native test.i
```
