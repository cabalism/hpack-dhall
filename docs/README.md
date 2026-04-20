## Hpack Dhall Docs

From the root of the repository, run:

### Watching

```
$ quarto preview docs
Preparing to preview

Watching files for changes
Browse at http://localhost:3054/
Listening on http://127.0.0.1:3054/
Opening in existing browser session.
GET: /
```

### Building

```
$ quarto render docs
[1/6] conditionals.qmd
[2/6] index.qmd
[3/6] translation.qmd
[4/6] phraseology.qmd
[5/6] linting.qmd
[6/6] cabal-commands.qmd

Output created: _site/index.html
```

### Deploying

```
$ git checkout main
... (make changes and commit on main)
$ quarto render docs
$ git checkout site
$ cp -a docs/_site/. .
... (commit changes brought over from main via copy from _site)
$ git push
```
