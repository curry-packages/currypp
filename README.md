The Curry Preprocessor
======================

This package contains the implementation of the
Curry preprocessor `currypp` which supports some
extensions for Curry programs, like

* integrated code, i.e., the integration of code
  written in some other language into Curry programs,
  like regular expressions, format specifications (`printf`),
  HTML and XML code,
* [default rules](http://doi.org/10.1017/S1471068416000168),
* [contracts](http://dx.doi.org/10.1007/978-3-642-27694-1_4).

Details about the usage can be found in the manual.
Here is a short summary how to use integrated code
in Curry programs.


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
* To preprocess SQL statements, you might add the option

       --model:<modelname>_SQLCode.info

  to use a non-standard info file for the database model


Writing files with integrated code:
-----------------------------------

The basic syntax of integrated code in Curry program looks like

    ``langtag expression''

Here, `langtag` is a tag indicating the kind of integrated language,
and `expression` is an expression of this language.

If enclosing accents are used in the expression itself,
the actual enclosing accents need to be of a higher number
than the inner graves, i.e., the following
integrated code expression is also allowed:

    ````langtag expression''''

The number of opening and closing accents must always be identical.

Currently, the following `langtag` values are supported:

* `format` - `printf` Syntax
* `printf` - same as above (but with an implicit `putStr` call)
* `regex`  - Polymorphic regex expressions
* `html`   - Standard HTML
* `xml`    - Standard XML
* `sql`    - SQL syntax

See the examples and source file comments for further details.
