# JSON Schema for Ada

This tool reads JSON Schema (Draft 04) and generates
* Ada types corresponding to each JSON Schema
* Subprograms to convert JSON into Ada value for these Ada types
* Subprograms to convert values Ada values to JSON

Currently the tool is able to generate Ada code for
[Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/).

## Build

You can build the tool with [Alire](https://alire.ada.dev):

    cd tools/json_schema
    alr build

Compiler will produce `.objs/tools/gen_json` executable.

## Usage

```
Usage: gen_json [options] <json_schema>.json

Where options:
  --root-package <package> - A package for generated types
  --enum-package <package> - A package for enumeration types
  --header-file  <file>    - A copyright header file

```

Run `gen_json` with JSON Schema file as an argument.
Generated code will be printed to `stdout`.
Use `gnatchop` and `gnatpp` to get compilable sources.
