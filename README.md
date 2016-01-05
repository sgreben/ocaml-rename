# ocaml-rename

An `.mli`-aware command-line renaming tool for OCaml projects, with `undo` and support for `ocamlfind` package dependencies.

**WARNING**: Renaming invariants are *not* checked, so name collisions can be introduced. However, the tool *does* have an undo feature to roll back the changes if this happens.

## Usage

```bash
ocaml-rename [options] [command] [source files in dependency order]
```

Currently, we can rename *values*, *types* and *record fields*. The following elements can't be renamed (yet): *modules*, *constructors*, *methods*.

## Example

Rename by identifier:

```bash
$ PROJECT_FILES=$(ocamldep -sort *.mli *.ml)
$ ocaml-rename value MyModule.oldName newName $PROJECT_FILES
  ...
```

Rename by location:

```bash
$ PROJECT_FILES=$(ocamldep -sort *.mli *.ml)
$ ocaml-rename value @mymodule.ml:4:9 newName $PROJECT_FILES
 ...
```

Undo:

```bash
$ PROJECT_FILES=$(ocamldep -sort *.mli *.ml)
$ ocaml-rename value @mymodule.ml:4:9 newName $PROJECT_FILES
 ...
 To undo, run:
    patch -p0 </tmp/ocaml_rename_f67771.bak.undo.diff
# Renaming done
$ patch -p0 </tmp/ocaml_rename_f67771.bak.undo.diff
# Renaming undone
```


#### Command syntax

All commands have the syntax [symbol type] [symbol] [new-name].  The currently supported symbol types are `value`, `type` and `field`.

| Command                        | Example                        |
|:-------------------------------|:-------------------------------|
| value at [value-loc] [new-id]  | value at module.ml:1:2 newName |
| value @[value-loc] [new-id]    | value @module.ml:1:2 newName   |
| value [value] [new-id]         | value Module.oldName newName   |
|                                |                                |
| type at [type-loc] [new-id]    | type at module.ml:1:2 newName  |
| type @[type-loc] [new-id]      | type @module.ml:1:2 newName    |
| type [type] [new-id]           | type Module.oldName newName    |
|                                |                                |
| field at [field-loc] [new-id]  | field at module.ml:1:2 newName |
| field @[field-loc] [new-id]    | field @module.ml:1:2 newName   |
| field [field] [new-id]         | field Module.oldName newName   |

A symbol can be specified either by location (using `at file:line:column`) or by its qualified identifier in the global environment (using `Module.symbol`).

#### Options

| Option            | Meaning                                                            |
|:------------------|:-------------------------------------------------------------------|
| -I [dir]          | Add [dir] to the list of include directories                       |
| -package [pkg]    | Find [pkg] using `ocamlfind query` and add its include directory   |
| --no-backup       | Disable backups (implies --no-undo) (default: make backups)        |
| --no-undo         | Disable undo patch generation (default: generate undo patch)       |
| --dry-run         | Perform a dry run (imples --no-backup, --no-undo) (default: false) |
| -v {0,1,2}        | Set verbosity level (default:1)                                    |
| -help, --help     | Display this list of options                                       |

## Build

Dependencies: OCaml `4.02.3`, ocamlfind `1.5.6`.

```bash
make main
```

## Test

```bash
make test
```