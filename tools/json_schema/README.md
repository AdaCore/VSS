# JSON Schema for Ada

This tool reads JSON Schema (Draft 04-07) and generates
* Ada types corresponding to each JSON Schema
* Subprograms to convert JSON into Ada value for these Ada types
* Subprograms to convert values Ada values to JSON

Currently the tool is able to generate Ada code for
[Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/)
and
[LSP Meta Model](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#metaModel)
(with a few minor changes).

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
  --holder <type:field>    - Use holder to break circular dependency

```

Run `gen_json` with JSON Schema file as an argument.
Generated code will be printed to `stdout`.
Use `gnatchop` and `gnatpp` to get compilable sources.

Option `--enum-package` is optional. If presented it make `gen_json` create
a nested package with all enumeration types. It may prevents name collisions
between types, subprograms and enumeration literals.

Option `--holder` provides a way to break circular dependency in manual mode.
It make `gen_json` generate a holder type and use it in each property pointed
by this option.

### Convert DAP JSON Schema

```sh
gen_json \
  --root-package DAP.Tools \
  --enum-package Enum \
  --header-file header \
  debugAdapterProtocol.json > file_to_chop
```

### Convert LSP metaModel JSON Schema

```sh
gen_json \
  --root-package LSP \
  --enum-package Enum \
  --header-file header \
  --holder "#/definitions/ArrayType:element" \
  --holder "#/definitions/MapType:value" \
  metaModel.schema.json > file_to_chop
```