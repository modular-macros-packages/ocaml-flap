## flap: Fused Lexing and Parsing

`flap` is a parsing library that fuses lexers and parsers together during code generation to improve performance.

The following paper has many more details:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;flap: A Deterministic Parser with Fused Lexing ([pdf][paper])  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Jeremy Yallop, Ningning Xie and Neel Krishnaswami  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;PLDI 2023  

*This branch is the MacoCaml port: the whole pipeline runs at
compile time as `$let` bindings (`Flap.Parse` and the layers beneath
it, `Normal`/`Normalize`/`Fused`/`Compiler`/`Recognizer`, are
templates over a `TERM` whose operations are `$let`s, laid out as
upstream's functors are), and generated parsers are
obtained by top-level splices instead of `Runnative.run`.  The
semantic-action combinator is upstream's `( $ )`; write it spaced
(`g $ f`), since a glued `$x` is a splice.  (`( $. )`, its name while
`$` was reserved, remains as a deprecated alias.)  See
PROPOSAL-macocaml-port.md for the design.*

## Trying out flap

There is a [Docker image][artifact] available on Zenodo with accompanying tutorial instructions for trying out `flap` and reproducing the results in the paper.

## Installation

1. Install the [BER MetaOCaml][ber-metaocaml] compiler using [OPAM][opam]:

   ```
   opam switch create 4.11.1+BER
   eval $(opam env)
   ```

2. Add the [metaocaml-opam][metaocaml-opam] repository:

   ```
   opam remote add metaocaml git+https://github.com/metaocaml/metaocaml-opam.git
   ```

3. Install the `flap` package:

   ```
   opam install flap
   ```
[paper]: https://www.cl.cam.ac.uk/~jdy22/papers/flap-a-deterministic-parser-with-fused-lexing.pdf
[opam]: https://opam.ocaml.org/
[artifact]: https://doi.org/10.5281/zenodo.7712770
[ber-metaocaml]: http://okmij.org/ftp/ML/MetaOCaml.html
[metaocaml-opam]: https://github.com/metaocaml/metaocaml-opam/
