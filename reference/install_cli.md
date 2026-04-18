# Install imuGAP CLI to PATH

Creates a symlink to the bundled CLI script so `imugap` is available as
a shell command.

## Usage

``` r
install_cli(path = "~/.local/bin")
```

## Arguments

- path:

  character; directory to install the symlink into. Defaults to
  `"~/.local/bin"`.

## Value

Invisible `TRUE` on success, errors on failure.
