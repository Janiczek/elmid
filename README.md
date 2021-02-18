# elmid

Like `ghcid`, but for Elm!

A CLI filesystem watcher that runs `elm make` automatically and gives you the errors grouped by files, in a collapsible format.

> :warning: Linux only, for now. See [issue #1](https://github.com/Janiczek/elmid/issues/1) for details.

![elmid in action](https://github.com/Janiczek/elmid/raw/main/doc/elmid.gif)

## Usage

```bash
$ elmid src/Main.elm
```

```bash
Usage: elmid [--elm-path ELM_PATH]
             [--watched-folder PATH]
             [--ignored-folder FOLDER]
             [--main-cwd PATH]
             MAIN_PATH
```

If in doubt, run `elmid --help`.

## Building

Have `stack` installed. Then cloning this repo and running `stack build` / `stack install` should be enough.

## TODO

- [ ] watch for creation of new directories and start watching them too
- [ ] ellipsis-clamp the path *from the left* so that the filename is always shown
- [ ] debounce
- [ ] after successful Elm compiler run, run `elm-review` too and display its errors similarly
- [ ] show the line number in the collapsed view
  - [ ] open editor by clicking at the line number: https://tracy.nette.org/en/open-files-in-ide
- [ ] check for `Debug.log` and warn user about them (should still be "All good" even if they're there)
