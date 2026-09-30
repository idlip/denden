# denden's own tasks (the example site). Run `just` from denden/ to list.

example_pub := "example/public"
example_elisp := "-Q -L . -l denden.el -l example/example-theme.el -l example/example-site.el"

# Build the example site into example/public/.
example-build:
    emacs --batch {{example_elisp}} --eval '(example-build t)'

# Build, then serve example/public/ locally: static-web-server, real or via `nix run`.
example-serve: example-build
    #!/usr/bin/env bash
    set -euo pipefail
    cd {{example_pub}}
    if command -v static-web-server >/dev/null 2>&1; then
        static-web-server --port 8000 --page404 404.html
    elif command -v nix >/dev/null 2>&1; then
        nix run nixpkgs#static-web-server -- --port 8000 --page404 404.html
    else
        python3 -m http.server 8000
    fi

# Delete the example's build output.
example-clean:
    rm -rf {{example_pub}}
