# Denden's own tasks, on the example site. Run `just` to list them.

public := "example/public"
port := "7777"
remote := "idlip"
example := "emacs --batch -Q -L . -L example -l example/example-site.el"

# Build the example site into example/public/.
build:
    {{example}} --eval '(denden-build example-site)'

# Serve example/public/ at http://localhost:7777. Run it in its own terminal.
web:
    #!/usr/bin/env bash
    set -euo pipefail
    if command -v static-web-server >/dev/null; then
        static-web-server --root {{public}} --port {{port}}
    else
        nix run nixpkgs#static-web-server -- --root {{public}} --port {{port}}
    fi

# Build, then publish the example on the gh-pages branch (https://denden.idlip.in/).
deploy: build
    #!/usr/bin/env bash
    set -euo pipefail
    worktree=.gh-pages-worktree
    if ! git show-ref --verify --quiet refs/heads/gh-pages; then
        # The first deploy starts gh-pages as an empty branch of its own.
        git branch gh-pages "$(git commit-tree "$(git hash-object -t tree /dev/null)" -m "gh-pages")"
    fi
    [ -d "$worktree" ] || git worktree add "$worktree" gh-pages
    # Exclude .git, not .git/: in a worktree, .git is a file.
    rsync -a --delete --exclude .git {{public}}/ "$worktree"/
    cd "$worktree"
    git add -A
    if git diff --cached --quiet; then
        echo "deploy: nothing changed"
    else
        git commit -m "deploy $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        git push {{remote}} gh-pages --force
    fi