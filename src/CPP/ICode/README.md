A translater from Curry with Integrated Code to standard Curry
==============================================================

Authors of the first version:

* Max Deppert - made@informatik.uni-kiel.de
* Jasper Sikorra - jsi@informatik.uni-kiel.de


General usage of the code integrator:
-------------------------------------

If the pre-processor is installed as the binary `currypp`,
Curry source files containing integrated code can be translated
by running `currypp` as follows:

    currypp <org-filename> <input-file> <output-file> foreigncode [-o]

The parameters are:

* The name of the original Curry source file.
* The name of the file containing the input to be translated.
* The name of the output file where the translated code should be stored.
* If the optional parameter `-o` is given, a copy of the translated code
  is stored in the file `org-filename.CURRYPP`.
* To preprocess SQL: --model:<modelname>_SQLCode.info   


Writing files with integrated code:
-----------------------------------

The basic syntax of integrated code in Curry program looks like

    ``langtag expression''

Here, `langtag` is a tag indicating the kind of integrated language,
and `expression` is an expression of this language.

If `` or '' are used in the expression itself, the enclosing accents
need to be of higher number than the inner graves, i.e., the following
integrated code expression is also allowed:

    ````langtag expression''''

The number of opening and closing accents must always be identical.

Currently, the following `langtag` values are supported:

format - `printf` Syntax
printf - same as above (but with an implicit `putStr` call)
regex  - Polymorphic regex expressions
html   - Standard HTML
xml    - Standard XML
sql    - SQL syntax

See the examples and source file comments for further details.
